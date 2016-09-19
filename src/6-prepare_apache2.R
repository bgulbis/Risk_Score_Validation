# prepare_apache2

library(edwr)
library(tidyverse)
library(lubridate)
library(stringr)
library(icd)
library(icuriskr)

data.tidy <- "data/tidy"

dirr::get_rds(data.tidy)

tmp_icu_stay <- select(icu_admit, pie.id, arrive.datetime, depart.datetime) %>%
    semi_join(patients_sampled, by = "pie.id")

labs <- c("sodium lvl", "potassium lvl", "co2", "creatinine lvl", "bun",
          "glucose lvl", "albumin lvl", "bilirubin total", "bili total",
          "poc a ph", "poc a pco2", "poc a po2", "wbc", "hct")

# how to use censored labs

data_labs <- tidy_labs %>%
    inner_join(tmp_icu_stay, by = "pie.id") %>%
    filter(lab.datetime >= arrive.datetime,
           lab.datetime <= arrive.datetime + hours(24),
           lab.datetime <= depart.datetime,
           lab %in% labs) %>%
    mutate(lab = str_replace_all(lab, " lvl", ""),
           lab = str_replace_all(lab, "poc a", "arterial"),
           lab = str_replace_all(lab, "bilirubin", "bili"),
           lab = str_replace_all(lab, " ", "_")) %>%
    select(pie.id, lab, lab.result) %>%
    group_by(pie.id, lab) %>%
    summarize_all(funs(min, max))

lab_min <- data_labs %>%
    select(-max) %>%
    spread(lab, min) %>%
    mutate(min = TRUE)

lab_max <- data_labs %>%
    select(-min) %>%
    spread(lab, max) %>%
    mutate(min = FALSE)

labs_min_max <- bind_rows(lab_min, lab_max) %>%
    rename(pco2 = arterial_pco2,
           ph = arterial_ph,
           pao2 = arterial_po2,
           scr = creatinine,
           hco3 = co2,
           bili = bili_total)

# labs_apache2 <- labs_min_max %>%
    # select(pie.id, min, pco2, ph, pao2, hco3, scr, hct:wbc)

vitals <- c("arterial systolic bp 1" = "sbp",
            "systolic blood pressure" = "sbp",
            "arterial diastolic bp 1" = "dbp",
            "diastolic blood pressure" = "dbp",
            "mean arterial pressure \\(invasive\\)" = "map",
            "mean arterial pressure" = "map",
            "spo2 percent" = "spo2",
            "apical heart rate" = "hr",
            "peripheral pulse rate" = "hr",
            "respiratory rate" = "rr",
            "temperature axillary" = "temp2",
            "temperature oral" = "temp2",
            "temperature bladder" = "temp",
            "temperature intravascular" = "temp",
            "temperature rectal" = "temp")

data_vitals <- tidy_vitals %>%
    inner_join(tmp_icu_stay, by = "pie.id") %>%
    filter(vital.datetime >= arrive.datetime,
           vital.datetime <= arrive.datetime + hours(24),
           vital.datetime <= depart.datetime) %>%
    mutate(vital = str_replace_all(vital, vitals)) %>%
    select(pie.id, vital, vital.result) %>%
    group_by(pie.id, vital) %>%
    summarize_all(funs(min, max))

vital_min <- data_vitals %>%
    select(-max) %>%
    spread(vital, min) %>%
    mutate(min = TRUE)

vital_max <- data_vitals %>%
    select(-min) %>%
    spread(vital, max) %>%
    mutate(min = FALSE)

# use rectal, bladder, or intravascular temp if available, if not, use any temp
vitals_min_max <- bind_rows(vital_min, vital_max) %>%
    mutate(temp = coalesce(temp, temp2)) %>%
    select(-temp2)


# tmp <- distinct(tidy_vent_settings, vent.event)

vent <- c("fio2 \\(%\\)" = "fio2",
          "spo2 percent" = "spo2",
          "oxygen therapy mode" = "mode",
          "invasive ventilation mode" = "mode",
          "poc a %fio2" = "fio2",
          "poc a peep" = "peep",
          "poc a po2" = "pao2",
          "poc a o2 sat" = "spo2",
          "non-invasive ventillation mode" = "mode")

data_vent <- tidy_vent_settings %>%
    inner_join(tmp_icu_stay, by = "pie.id") %>%
    filter(vent.datetime >= arrive.datetime,
           vent.datetime <= arrive.datetime + hours(24),
           vent.datetime <= depart.datetime) %>%
    mutate(vent.event = str_replace_all(vent.event, vent)) %>%
    filter(vent.event == "fio2") %>%
    select(pie.id, vent.event, vent.result) %>%
    group_by(pie.id, vent.event) %>%
    summarize_all(funs(max)) %>%
    spread(vent.event, vent.result)

data_gcs <- tidy_icu_scores %>%
    inner_join(tmp_icu_stay, by = "pie.id") %>%
    filter(assess.datetime >= arrive.datetime,
           assess.datetime <= arrive.datetime + hours(24),
           assess.datetime <= depart.datetime,
           assessment == "glasgow coma score") %>%
    group_by(pie.id) %>%
    summarize(gcs = min(assess.result))

# identify patients with ARF
# change from < 1.4 to > 2 or initial > 2 which returned to normal (< 1.4)
arf_apache2 <- tidy_labs %>%
    semi_join(patients_sampled, by = "pie.id") %>%
    inner_join(tmp_icu_stay, by = "pie.id") %>%
    filter(lab == "creatinine lvl") %>%
    arrange(pie.id, lab.datetime) %>%
    group_by(pie.id) %>%
    mutate(normal_baseline = lab.datetime <= arrive.datetime + hours(24) & lab.result < 1.4,
           arf1 = normal_baseline == TRUE & lab.datetime <= arrive.datetime + hours(24) & lab.result > 2,
           high_baseline = first(lab.result) > 2,
           arf2 = high_baseline == TRUE & lab.result < 1.4) %>%
    summarize(arf1 = sum(arf1),
              arf2 = sum(arf2)) %>%
    mutate(arf = arf1 > 0 | arf2 > 0)

# identify surgery patients
data_surgery <- tidy_surgeries %>%
    select(-priority, -`asa class`, -surgery) %>%
    left_join(tidy_visits[c("pie.id", "admit.datetime", "admit.type")], by = "pie.id") %>%
    left_join(tmp_icu_stay, by = "pie.id") %>%
    filter(primary.proc == TRUE) %>%
    arrange(pie.id, surg.start.datetime) %>%
    distinct(pie.id, .keep_all = TRUE) %>%
    mutate(surg_admit = surg.start.datetime <= admit.datetime + hours(24),
           surg_icu = surg.start.datetime >= arrive.datetime - hours(24) &
               arrive.datetime <= surg.stop.datetime + hours(4),
           elective = add.on == FALSE &
               !str_detect(asa.class, "E") &
               admit.type != "Emergency" &
               surg_admit == TRUE) %>%
    filter(surg_icu == TRUE)

# find MDC for each MS-DRG
mdc <- readr::read_csv("data/external/msdrg_to_mdc.csv") %>%
    dmap_at("mdc", str_replace_all, pattern = "No MDC|MDC ", replacement = "")

msdrg <- tidy_codes_drg %>%
    left_join(mdc[c("msdrg", "mdc")], by = c("drg" = "msdrg"))

data_comorbidities_icd9 <- tidy_codes_icd %>%
    filter(icd9 == TRUE) %>%
    icd_comorbid(comorbidity_map_icd9,
                 visit_name = "pie.id",
                 icd_name = "diag.code",
                 short_code = FALSE,
                 return_df = TRUE)

apache2_comorbid <- data_comorbidities_icd9 %>%
    transmute(pie.id = pie.id,
              liver = (portal_htn & (cirrhosis | upper_gi_bleed)) | encephalopathy_coma,
              cardiovasc = chf,
              respiratory = pulmonary | pvd | hypoxia | hypercapnia | polycythemia | pulm_htn | resp_depend,
              renal = chronic_hd,
              immunocomp = immunosuppress | chemo | radiation | steroids | leukemia | lymphoma | (hiv & (kaposi | pneumocytosis | toxoplasmosis | tuberculosis))) %>%
    mutate(comorbidity = liver | cardiovasc | respiratory | renal | immunocomp)

data_apache2 <- inner_join(labs_min_max, vitals_min_max, by = c("pie.id", "min")) %>%
    left_join(data_vent, by = "pie.id") %>%
    left_join(data_gcs, by = "pie.id") %>%
    left_join(tidy_demographics[c("pie.id", "age")], by = "pie.id") %>%
    left_join(arf_apache2[c("pie.id", "arf")], by = "pie.id") %>%
    left_join(data_surgery[c("pie.id", "elective")], by = "pie.id") %>%
    left_join(apache2_comorbid[c("pie.id", "comorbidity")], by = "pie.id") %>%
    mutate_if(is.character, as.numeric) %>%
    mutate(fio2 = coalesce(fio2, 21),
           aa_grad = aa_gradient(pco2, pao2, fio2, F_to_C(temp), 13.106),
           admit = if_else(elective == FALSE, "elective", "emergency", "nonoperative"))

score_apache2 <- apache2(data_apache2)

# saveRDS(apache_test, "data/external/data_apache2.Rds")
