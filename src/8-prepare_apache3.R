# prepare apache3

source("src/7-prepare_saps2.R")

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

map_comorbidity3 <- function(df) {
    dots <- list("pie.id", "aids", "hepatic_failure", "lymphoma", "cancer_mets",
                 "leukemia", "mult_myeloma", "immunosuppress", "cirrhosis")

    df %>%
        dplyr::mutate_(.dots = purrr::set_names(
            x = list(~hiv & (kaposi | lymphoma | pneumocytosis | toxoplasmosis | tuberculosis)),
            nm = "aids")) %>%
        dplyr::select_(.dots = dots) %>%
        tidyr::gather_("comorbidity", "val", gather_cols = purrr::flatten_chr(dots)[-1]) %>%
        dplyr::filter_(.dots = list(~val == TRUE))
}

apache3_comorbidity <- function(df) {
    x <- df$comorbidity

    df$score <- dplyr::case_when(
        x == "aids" ~ 6,
        x == "hepatic_failure" ~ 5,
        x == "lymphoma" ~ 4,
        x == "cancer_mets" ~ 3,
        x == "leukemia" | x == "mult_myeloma" | x == "immunosuppress" ~ 2,
        x == "cirrhosis" ~ 1,
        is.character(x) ~ 0
    )

    df %>%
        dplyr::arrange_(.dots = list("pie.id", ~dplyr::desc(score))) %>%
        dplyr::distinct_(.dots = list("pie.id"), .keep_all = TRUE)
}

get_hd <- function(df) {
    dplyr::select_(df, .dots = list("pie.id", "chronic_hd"))
}

tmp_apache3_comorbidity <- map(data_comorbidities, map_comorbidity3) %>%
    map(apache3_comorbidity)

tmp_manual <- manual_data %>%
    spread(comorbidity, value) %>%
    select(pie.id, aids, hepatic_failure, lymphoma, cancer_mets, leukemia,
           mult_myeloma, immunosuppress, cirrhosis) %>%
    gather(comorbidity, val, -pie.id) %>%
    filter(val == TRUE) %>%
    apache3_comorbidity() %>%
    select(pie.id, comorbidity)

manual_hd <- manual_data %>%
    filter(comorbidity == "chronic_hd") %>%
    spread(comorbidity, value)

# need to check for chronic HD
data_apache3 <- inner_join(labs_min_max, vitals_min_max, by = c("pie.id", "min")) %>%
    left_join(tidy_demographics[c("pie.id", "age")], by = "pie.id") %>%
    left_join(data_vent, by = "pie.id") %>%
    left_join(vent, by = "pie.id") %>%
    left_join(uop, by = "pie.id") %>%
    left_join(gcs_apache3, by = "pie.id") %>%
    left_join(data_surgery[c("pie.id", "elective")], by = "pie.id") %>%
    mutate_if(is.character, as.numeric) %>%
    mutate(fio2 = coalesce(fio2, 21),
           aa_grad = aa_gradient(pco2, pao2, fio2, F_to_C(temp), 13.106),
           admit = if_else(elective == FALSE, "elective", "emergency", "nonoperative"))

apache3_man <- left_join(data_apache3, tmp_manual, by = "pie.id") %>%
    left_join(manual_hd, by = "pie.id")

apache3_icd <- map(tmp_apache3_comorbidity, ~left_join(data_apache3, .x, by = "pie.id")) %>%
    map2(map(data_comorbidities, get_hd), ~left_join(.x, .y, by = "pie.id"))

apache3_icd <- c(apache3_icd, list(apache3_man)) %>%
    map(~mutate(.x, arf = scr >= 1.5 & uop < 410 & (is.na(chronic_hd) | chronic_hd == FALSE)))

score_apache3 <- map(apache3_icd, apache3)

walk2(apache3_icd, comorbid_names, ~saveRDS(.x, file = paste0("data/final/data_apache3_", .y, ".Rds")))
walk2(score_apache3, comorbid_names, ~saveRDS(.x, file = paste0("data/final/score_apache3_", .y, ".Rds")))

