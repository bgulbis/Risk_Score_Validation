# 1-screen.R

library(edwr)
library(dplyr)
library(lubridate)

data.raw <- "data/raw"

# Run EDW query: Patients - by Unit Admission
#   * Person Location - Nurse Unit (To):
#       - Cullen 2 E Medical Intensive Care Unit
#       - HVI Cardiovascular Intensive Care Unit
#       - HVI Cardiac Care Unit
#       - Hermann 3 Shock Trauma Intensive Care Unit
#       - Hermann 3 Transplant Surgical ICU
#       - Jones 7 J Elective Neuro ICU
#       - Jones 7 Neuro Trauma ICU
#   * Person Location- Facility (Curr): Memorial Hermann Hospital
#   * Admit date range: User-Defined
#   * Admit begin: 7/1/2014 00:00:00
#   * Admit end: 7/1/2016 00:00:00

screen <- read_data(data.raw, "screen") %>%
    as.patients() %>%
    filter(discharge.datetime <= ymd("2016-06-30", tz = "US/Central"),
           age >= 18) %>%
    arrange(pie.id)

pie <- concat_encounters(screen$pie.id, 910)

# Run EDW Query: Demographics
#   * PowerInsight Encounter Id: all values from object pie

demograph <- read_data(data.raw, "demographics") %>%
    as.demographics()

# remove any discharge to court/law
excl.prison <- demograph %>%
    filter(disposition %in% c("DC/TF TO COURT/LAW", "Court/Law Enforcement"))

demograph <- anti_join(demograph, excl.prison, by = "pie.id")

# keep only the first admission for any individual patient
include <- demograph %>%
    group_by(person.id) %>%
    inner_join(screen, by = "pie.id") %>%
    arrange(person.id, discharge.datetime) %>%
    summarize(pie.id = first(pie.id)) %>%
    arrange(pie.id)

pie2 <- concat_encounters(include$pie.id)

# Run EDW Query: Diagnosis Codes (ICD-9/10-CM) - All
#   * PowerInsight Encounter Id: all values from object pie2
icd.codes <- read_data(data.raw, "diagnosis") %>%
    as.diagnosis()

female <- demograph %>%
    semi_join(include, by = "pie.id") %>%
    filter(sex == "Female") %>%
    arrange(pie.id)

pie3 <- concat_encounters(female$pie.id)

# Run EDW Query: Labs - Pregnancy
#   * PowerInsight Encounter Id: all values from object pie3

preg.lab <- read_data(data.raw, "preg") %>%
    as.labs() %>%
    check_pregnant()

excl.preg <- icd.codes %>%
    semi_join(female, by = "pie.id") %>%
    check_pregnant() %>%
    full_join(preg.lab, by = "pie.id") %>%
    distinct

include <- anti_join(include, excl.preg, by = "pie.id")

exclude <- list(screen = nrow(screen),
                prisoners = nrow(excl.prison),
                pregnant = nrow(excl.preg))

saveRDS(include, "data/tidy/include.Rds")
saveRDS(exclude, "data/tidy/exclude.Rds")

# queries to run: demographics; diagnosis codes; labs - abg, cbc, chemistry,
# lfts, pregnancy; location history; measures; urine output; ventilator data -
# settings, start and stop; vitals; icu assessments; surgeries?

# need to add: VBGs; components of GCS

# apply exclusion criteria; randomly select 100 each before and after Oct 1, 2015
# (ICD-10 implementation)
