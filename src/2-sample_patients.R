# sample_patients.R

library(edwr)
library(dplyr)

data.raw <- "data/raw"

include <- readRDS("data/tidy/include.Rds")

icd.codes <- read_data(data.raw, "diagnosis") %>%
    as.diagnosis() %>%
    tidy_data() %>%
    semi_join(include, by = "pie.id")

# separate patients with ICD-9 and ICD-10 codes and sample evenly from both
num <- 900

set.seed(7708)
icd9 <- icd.codes %>%
    filter(icd9 == TRUE) %>%
    distinct(pie.id) %>%
    sample_n(num)

set.seed(12312)
icd10 <- icd.codes %>%
    filter(icd9 == FALSE) %>%
    distinct(pie.id) %>%
    sample_n(num)

pts.sample <- bind_rows(icd9, icd10) %>%
    arrange(pie.id)

pie <- concat_encounters(pts.sample$pie.id)

# Run EDW Queries:
#   - DRG Codes - All
#   - ICU Assessments (CAM-ICU, GCS, RASS)
#   - Identifiers - by PowerInsight Encounter Id
#   - Labs - ABG
#   - Labs - CBC
#   - Labs - Chemistry
#   - Labs - LFTs
#   - Location History
#   - Measures (Heigh and Weight)
#   - Procedure Codes (ICD-9-CM/ICD-10-PCS) - All
#   - Surgeries
#   - Urine Output
#   - Ventilator Data - Settings
#   - Ventilator Data - Start and Stop
#   - Vitals
#
# Using the following parameters:
#   * PowerInsight Encounter Id: all values from object pie

