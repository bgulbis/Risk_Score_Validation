# calculate apache3

source("src/6-calculate_saps2.R")

# labs_apache3 <- bind_rows(lab_min, lab_max) %>%
#     select(pie.id, min, albuminpco2, ph, pao2, hco3, scr, hct:wbc)
# select(pie.id, min, albumin, glucose, bili)

gcs_apache3 <- tidy_icu_scores %>%
    inner_join(tmp_icu_stay, by = "pie.id") %>%
    filter(assess.datetime >= arrive.datetime,
           assess.datetime <= arrive.datetime + hours(24),
           assess.datetime <= depart.datetime,
           assessment != "glasgow coma score",
           str_detect(assessment, "glasgow")) %>%
    mutate(score = as.numeric(str_extract(assess.result, "[0-9]{1,2}")),
           assess = str_extract(assessment, "(motor)|(verbal)|(eye)")) %>%
    group_by(pie.id, assess.datetime, assess) %>%
    summarize(score = min(score)) %>%
    spread(assess, score) %>%
    ungroup() %>%
    mutate(neuro = apache3_neuro(eye, motor, verbal)) %>%
    group_by(pie.id) %>%
    summarize(neuro = max(neuro))

tmp_comorbid <- data_comorbidities_icd9 %>%
    mutate(aids = hiv & (kaposi | lymphoma | pneumocytosis | toxoplasmosis | tuberculosis)) %>%
    select(pie.id, aids, hepatic_failure, lymphoma, cancer_mets, leukemia, mult_myeloma, immunosuppress, cirrhosis) %>%
    gather(comorbidity, val, -pie.id) %>%
    filter(val == TRUE)

z <- tmp_comorbid$comorbidity

tmp_comorbid$score <- case_when(
    z == "aids" ~ 6,
    z == "hepatic_failure" ~ 5,
    z == "lymphoma" ~ 4,
    z == "cancer_mets" ~ 3,
    z == "leukemia" | z == "mult_myeloma" | z == "immunosuppress" ~ 2,
    z == "cirrhosis" ~ 1,
    is.character(z) ~ 0
)

apache3_comorbid <- tmp_comorbid %>%
    arrange(pie.id, score) %>%
    distinct(pie.id, .keep_all = TRUE)

# need to check for chronic HD
data_apache3 <- inner_join(labs_min_max, vitals_min_max, by = c("pie.id", "min")) %>%
    left_join(tidy_demographics[c("pie.id", "age")], by = "pie.id") %>%
    left_join(data_vent, by = "pie.id") %>%
    left_join(vent, by = "pie.id") %>%
    left_join(uop, by = "pie.id") %>%
    left_join(gcs_apache3, by = "pie.id") %>%
    left_join(data_surgery[c("pie.id", "elective")], by = "pie.id") %>%
    mutate_if(is.character, as.numeric) %>%
    left_join(apache3_comorbid[c("pie.id", "comorbidity")], by = "pie.id") %>%
    mutate(fio2 = coalesce(fio2, 21),
           aa_grad = aa_gradient(pco2, pao2, fio2, F_to_C(temp), 13.106),
           arf = scr >= 1.5 & uop < 410,
           admit = if_else(elective == FALSE, "elective", "emergency", "nonoperative"))

# saveRDS(gcs_apache3, "data/external/apache3_neuro_test.Rds")

score_apache3 <- apache3(data_apache3)