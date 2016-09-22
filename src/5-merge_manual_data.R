# merge_manual_data

library(readxl)
library(edwr)
library(tidyverse)
library(stringr)

data.external <- "data/external"

identifiers <- read_data(data.external, "^identifiers") %>%
    as.id()

comorbid <- c("Cirrhosis" = "cirrhosis",
              "Upper GI bleeding" = "upper_gi_bleed",
              "Hepatic failure" = "hepatic_failure",
              "Encephalopathy" = "encephalopathy_coma",
              "Coma" = "encephalopathy_coma",
              "Heart failure" = "chf",
              "Chronic restrictive, obstructive, or vascular disease" = "pulmonary",
              "Chronic hypoxia" = "hypoxia",
              "Hypercapnia" = "hypercapnia",
              "Secondary polycythemia" = "polycythemia",
              "Pulmonary hypertension" = "pulm_htn",
              "Respiratory dependency" = "resp_depend",
              "Acute renal failure" = "arf",
              "Receiving chronic dialysis" = "chronic_hd",
              "Metastatic cancer" = "cancer_mets",
              "Immunosuppression" = "immunosuppress",
              "Chemotherapy" = "chemo",
              "Radiation" = "radiation",
              "Long-term or high-dose steroids" = "steroids",
              "Leukemia" = "leukemia",
              "Multiple myeloma" = "mult_myeloma",
              "Lymphoma" = "lymphoma",
              "AIDS" = "aids")

manual_data <- read_excel(paste(data.external, "2016-09-22_manual_data.xlsx", sep = "/"),
                     col_types = c("text", "text", "numeric", "text")) %>%
    rename(fin = `Patient ID`,
           comorbidity = `Co-morbidity`,
           value = Value,
           comments = Comments) %>%
    mutate(value = if_else(value == 1, TRUE, FALSE, NA),
           comorbidity = str_replace_all(comorbidity, comorbid)) %>%
    filter(!is.na(fin)) %>%
    left_join(identifiers, by = "fin") %>%
    select(pie.id, comorbidity, value) %>%
    arrange(pie.id, comorbidity, desc(value)) %>%
    distinct(pie.id, comorbidity, .keep_all = TRUE)

manual_patients <- distinct(manual_data, pie.id)

saveRDS(manual_data, "data/tidy/manual_data.Rds")
saveRDS(manual_patients, "data/final/manual_patients.Rds")
