# filter_data_icu_admit

library(edwr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

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
           lab %in% labs) %>%
    mutate(lab = str_replace_all(lab, " lvl", ""),
           lab = str_replace_all(lab, "poc a", "arterial"),
           lab = str_replace_all(lab, "bilirubin", "bili"),
           lab = str_replace_all(lab, " ", "_")) %>%
    select(pie.id, lab, lab.result) %>%
    group_by(pie.id, lab) %>%
    summarize_all(funs(min, max))
    # spread(lab, lab.result)

lab_min <- data_labs %>%
    select(-max) %>%
    spread(lab, min)

lab_max <- data_labs %>%
    select(-min) %>%
    spread(lab, max)

labs_min_max <- bind_rows(lab_min, lab_max)
