# calculate apache3

source("src/6-calculate_saps2.R")

labs_min_max_aps3 <- bind_rows(lab_min, lab_max) %>%
    rename(bili = bili_total) %>%
    select(pie.id, min, albumin, glucose, bili)


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

# need to check for chronic HD
apache3_test <- apache_test %>%
    select(-hco3, -potassium, -gcs, -surgical_status) %>%
    left_join(labs_min_max_aps3, by = c("pie.id", "min")) %>%
    left_join(vent, by = "pie.id") %>%
    left_join(uop, by = "pie.id") %>%
    left_join(gcs_apache3, by = "pie.id") %>%
    mutate(aa_grad = aa_gradient(pco2, pao2, fio2, F_to_C(temp), 13.106),
           arf = scr >= 1.5 & uop < 410)

saveRDS(gcs_apache3, "data/external/apache3_neuro_test.Rds")
