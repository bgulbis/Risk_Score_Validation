# filter_data_icu_admit

library(edwr)
library(dplyr)

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
    distinct(.keep_all = TRUE)

excl_no_icu <- anti_join(patients_sampled, data_icu_admit, by = "pie.id")

eligible <- semi_join(patients_sampled, data_icu_admit, by = "pie.id")
