# filter_data_icu_admit

library(edwr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

data.tidy <- "data/tidy"

dirr::get_rds(data.tidy)

# list of ICU's
icu <- c("HVI Cardiovascular Intensive Care Unit",
         "Cullen 2 E Medical Intensive Care Unit",
         "Jones 7 J Elective Neuro ICU",
         "Hermann 3 Shock Trauma Intensive Care Unit",
         "Hermann 3 Transplant Surgical ICU",
         "HVI Cardiac Care Unit")

data_icu_admit <- tidy_locations %>%
    filter(location %in% icu) %>%
    arrange(pie.id, arrive.datetime) %>%
    group_by(pie.id) %>%
    distinct(.keep_all = TRUE) %>%
    filter(unit.length.stay > 0)

excl_icu <- anti_join(patients_sampled, data_icu_admit, by = "pie.id")

eligible <- semi_join(patients_sampled, data_icu_admit, by = "pie.id")

tmp_icu_stay <- select(data_icu_admit, pie.id, arrive.datetime, depart.datetime)

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
