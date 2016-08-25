# sample_patients.R

library(edwr)
library(dplyr)

data_raw <- "data/raw"

eligible <- readRDS("data/tidy/eligible.Rds")

icd_codes <- read_data(data_raw, "diagnosis") %>%
    as.diagnosis() %>%
    tidy_data() %>%
    semi_join(eligible, by = "pie.id") %>%
    distinct(pie.id, icd9)

excl_icd <- icd_codes %>%
    group_by(pie.id) %>%
    summarize(n = n()) %>%
    filter(n > 1)

icd_codes <- anti_join(icd_codes, excl_icd, by = "pie.id")

# separate patients with ICD-9 and ICD-10 codes and sample evenly from both
num <- 900

set.seed(7708)
icd9 <- icd_codes %>%
    filter(icd9 == TRUE) %>%
    distinct(pie.id) %>%
    sample_n(num)

set.seed(12312)
icd10 <- icd_codes %>%
    filter(icd9 == FALSE) %>%
    distinct(pie.id) %>%
    sample_n(num)

pts_sample <- bind_rows(icd9, icd10) %>%
    arrange(pie.id)

pie <- concat_encounters(pts_sample$pie.id)

saveRDS(pts_sample, "data/tidy/patients_sampled.Rds")

# Run EDW Queries:
#   - DRG Codes - All
#   - ICU Assessments (CAM-ICU, GCS, RASS)
#   - Labs - ABG
#   - Labs - CBC
#   - Labs - Chemistry
#   - Labs - LFTs
#   - Location History
#   - Measures (Height and Weight)
#   - Procedure Codes (ICD-9-CM/ICD-10-PCS) - All
#   - Surgeries
#   - Urine Output
#   - Ventilator Data - Settings
#   - Ventilator Data - Start and Stop
#   - Visit Data
#   - Vitals
#
# Using the following parameters:
#   * PowerInsight Encounter Id: all values from object pie

