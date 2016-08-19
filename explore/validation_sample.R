# validation_sample.R

library(edwr)
library(dplyr)

data.raw <- "data/raw"

include <- readRDS("data/tidy/include.Rds")

icd.codes <- read_data(data.raw, "diagnosis") %>%
    as.diagnosis() %>%
    semi_join(include, by = "pie.id")

# separate patients with ICD-9 and ICD-10 codes

set.seed(7708)
icd9 <- icd.codes %>%
    filter(code.source == "ICD-9-CM") %>%
    distinct(pie.id) %>%
    sample_n(5)

set.seed(12312)
icd10 <- icd.codes %>%
    filter(code.source == "ICD-10-CM") %>%
    distinct(pie.id) %>%
    sample_n(5)

pts.sample <- bind_rows(icd9, icd10)

pie <- concat_encounters(pts.sample$pie.id)
print(pie)

# Run EDW Query: Identifiers - by PowerInsight Encounter Id
#   * PowerInsight Encounter Id: all values from object pie

fins <- read_data("data/external", "edw") %>%
    as.id()

readr::write_csv(fins["fin"], "data/external/validation_sample.csv")
