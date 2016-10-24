# prepare_demographics

library(tidyverse)

data.tidy <- "data/tidy"

dirr::get_rds(data.tidy)

icu <- c("Jones 7 J Elective Neuro ICU",
         "Hermann 3 Transplant Surgical ICU",
         "Cullen 2 E Medical Intensive Care Unit",
         "Hermann 3 Shock Trauma Intensive Care Unit",
         "HVI Cardiovascular Intensive Care Unit",
         "HVI Cardiac Care Unit",
         "Jones 7 Neuro Trauma ICU")

icu_location <- tidy_locations %>%
    filter(location %in% icu) %>%
    arrange(pie.id, arrive.datetime) %>%
    distinct(pie.id, .keep_all = TRUE)

data_demographics <- tidy_demographics %>%
    select(pie.id:length.stay) %>%
    left_join(icu_location[c("pie.id", "location")], by = "pie.id")

saveRDS(data_demographics, "data/final/data_demographics.Rds")
