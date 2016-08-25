# tidy_data.R

library(edwr)
library(dplyr)

data.raw <- "data/raw"
data.tidy <- "data/tidy"

patients <- readRDS("data/tidy/patients_sampled.Rds")

tidy_demographics <- read_data(data.raw, "demograph") %>%
    as.demographics() %>%
    semi_join(patients, by = "pie.id")

tidy_codes_icd <- read_data(data.raw, "diagnosis") %>%
    as.diagnosis() %>%
    tidy_data() %>%
    semi_join(patients, by = "pie.id")

tidy_labs <- read_data(data.raw, "labs") %>%
    as.labs() %>%
    tidy_data() %>%
    semi_join(patients, by = "pie.id")

tidy_locations <- read_data(data.raw, "locations") %>%
    as.locations() %>%
    tidy_data() %>%
    semi_join(patients, by = "pie.id")

tidy_measures <- read_data(data.raw, "measures") %>%
    as.measures() %>%
    semi_join(patients, by = "pie.id")

tidy_procedures <- read_data(data.raw, "procedures") %>%
    as.procedures() %>%
    semi_join(patients, by = "pie.id")

tidy_surgeries <- read_data(data.raw, "surgeries") %>%
    as.surgeries() %>%
    semi_join(patients, by = "pie.id")

tidy_uop <- read_data(data.raw, "uop") %>%
    as.uop() %>%
    semi_join(patients, by = "pie.id")

tidy_visits <- read_data(data.raw, "visit") %>%
    as.visits() %>%
    semi_join(patients, by = "pie.id")

tidy_vent_times <- read_data(data.raw, "vent-times") %>%
    as.vent_times() %>%
    tidy_data(dc = tidy_visits) %>%
    semi_join(patients, by = "pie.id")

dirr::save_rds(data.tidy, "^tidy_")
