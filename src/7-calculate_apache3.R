# calculate apache3

source("src/6-calculate_saps2.R")

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

saveRDS(gcs_apache3, "data/external/apache3_neuro_test.Rds")
