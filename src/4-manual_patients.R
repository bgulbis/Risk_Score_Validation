# manual_patients

library(edwr)
library(dplyr)

data.tidy <- "data/tidy"

dirr::get_rds(data.tidy)

print(concat_encounters(patients_sampled$pie.id))

# Run EDW Query:
#   * Identifiers - by PowerInsight Encounter Id
#       - all values from patients_sampled

