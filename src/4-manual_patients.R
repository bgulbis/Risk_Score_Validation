# manual_patients

library(edwr)
library(dplyr)
library(readr)

data.tidy <- "data/tidy"
data.external <- "data/external"

dirr::get_rds(data.tidy)

print(concat_encounters(patients_sampled$pie.id))

# Run EDW Query:
#   * Identifiers - by PowerInsight Encounter Id
#       - all values from patients_sampled

identifiers <- read_data(data.external, "^identifiers") %>%
    as.id()

# create list of FINs for manual review and save to csv

manual_review <- select(icu_admit, pie.id, arrive.datetime, depart.datetime) %>%
    semi_join(patients_sampled, by = "pie.id") %>%
    inner_join(identifiers, by = "pie.id") %>%
    ungroup() %>%
    select(fin, arrive.datetime, depart.datetime)

write_csv(manual_review, paste(data.external, "manual_review.csv", sep = "/"))
