# prepare_saps2

source("src/6-prepare_apache2.R")

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

saps2_comorbidity <- function(df) {
    df %>%
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(~hiv & (kaposi | lymphoma | pneumocytosis | toxoplasmosis | tuberculosis),
                     ~leukemia | lymphoma | mult_myeloma),
            nm = list("aids", "heme"))) %>%
        dplyr::select_(.dots = list("pie.id", "aids", "heme", "cancer_mets")) %>%
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(
                ~dplyr::if_else(aids, "aids",
                                dplyr::if_else(heme, "heme",
                                               dplyr::if_else(cancer_mets, "cancer_mets", "none", "none"),
                                               "none"),
                                "none")
            ),
            nm = "comorbidity"
        )) %>%
        dplyr::select_(.dots = list("pie.id", "comorbidity"))
}

tmp_saps2_comorbidity <- map(data_comorbidities, saps2_comorbidity)

saps2_manual <- manual_data %>%
    spread(comorbidity, value) %>%
    mutate(heme = leukemia | lymphoma | mult_myeloma) %>%
    select(pie.id, aids, heme, cancer_mets) %>%
    mutate(comorbidity = if_else(aids, "aids",
                                 if_else(heme, "heme",
                                         if_else(cancer_mets, "cancer_mets", "none")))) %>%
    select(pie.id, comorbidity)

tmp_saps2_comorbidity <- c(tmp_saps2_comorbidity, list(saps2_manual))

data_saps2 <- inner_join(labs_min_max, vitals_min_max, by = c("pie.id", "min")) %>%
    left_join(data_vent, by = "pie.id") %>%
    left_join(data_gcs, by = "pie.id") %>%
    left_join(tidy_demographics[c("pie.id", "age")], by = "pie.id") %>%
    left_join(uop, by = "pie.id") %>%
    left_join(data_surgery[c("pie.id", "elective")], by = "pie.id") %>%
    left_join(vent, by = "pie.id") %>%
    mutate_if(is.character, as.numeric) %>%
    mutate(fio2 = coalesce(fio2, 21),
           admit_type = if_else(elective == FALSE, "elective", "emergency", "nonoperative")) %>%
    select(-dbp, -map, -rr, -spo2, -elective)

saps2_icd <- map(tmp_saps2_comorbidity, ~left_join(data_saps2, .x, by = "pie.id"))
score_saps2 <- map(saps2_icd, saps2)

walk2(saps2_icd, comorbid_names, ~saveRDS(.x, file = paste0("data/final/data_saps2_", .y, ".Rds")))
walk2(score_saps2, comorbid_names, ~saveRDS(.x, file = paste0("data/final/score_saps2_", .y, ".Rds")))
