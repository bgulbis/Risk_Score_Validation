# calculate_saps2

source("src/5-calculate_apache2.R")

# labs_saps2 <- bind_rows(lab_min, lab_max) %>%
#     rename(pco2 = arterial_pco2,
#            ph = arterial_ph,
#            pao2 = arterial_po2,
#            scr = creatinine,
#            hco3 = co2,
#            bili = bili_total) %>%
#     select(pie.id, min, pao2:hco3, potassium:wbc)

uop <- tidy_uop %>%
    inner_join(tmp_icu_stay, by = "pie.id") %>%
    filter(uop != "urine count",
           uop.datetime >= arrive.datetime,
           uop.datetime <= arrive.datetime + hours(24)) %>%
    dmap_at("uop.result", as.numeric) %>%
    group_by(pie.id) %>%
    summarize(uop = sum(uop.result, na.rm = TRUE) / 1000)

vent <- tidy_vent_times %>%
    inner_join(tmp_icu_stay, by = "pie.id") %>%
    mutate(vent = int_overlaps(interval(start.datetime, stop.datetime),
                               interval(arrive.datetime, arrive.datetime + hours(24)))) %>%
    group_by(pie.id) %>%
    summarize(vent = sum(vent, na.rm = TRUE)) %>%
    mutate(vent = vent >= 1)

tmp_comorbid <- data_comorbidities_icd9 %>%
    mutate(aids = hiv & (kaposi | lymphoma | pneumocytosis | toxoplasmosis | tuberculosis),
           heme = leukemia | lymphoma | mult_myeloma) %>%
    select(pie.id, aids, heme, cancer_mets) %>%
    gather(comorbidity, val, -pie.id) %>%
    filter(val == TRUE)

z <- tmp_comorbid$comorbidity

tmp_comorbid$score <- case_when(
    z == "aids" ~ 3,
    z == "heme" ~ 2,
    z == "cancer_mets" ~ 1,
    is.character(z) ~ 0
)

saps2_comorbid <- tmp_comorbid %>%
    arrange(pie.id, score) %>%
    distinct(pie.id, .keep_all = TRUE)

saps2_test <- inner_join(labs_min_max, vitals_min_max, by = c("pie.id", "min")) %>%
    left_join(data_vent, by = "pie.id") %>%
    left_join(data_gcs, by = "pie.id") %>%
    left_join(tidy_demographics[c("pie.id", "age")], by = "pie.id") %>%
    left_join(uop, by = "pie.id") %>%
    left_join(data_surgery[c("pie.id", "elective")], by = "pie.id") %>%
    left_join(vent, by = "pie.id") %>%
    mutate_if(is.character, as.numeric) %>%
    left_join(saps2_comorbid[c("pie.id", "comorbidity")], by = "pie.id") %>%
    mutate(fio2 = coalesce(fio2, 21),
           admit_type = if_else(elective == FALSE, "elective", "emergency", "nonoperative")) %>%
    select(-dbp, -map, -rr, -spo2, -elective)

saveRDS(saps2_test, "data/external/saps2_test.Rds")

saps2_score <- saps2(saps2_test)
