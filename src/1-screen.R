# 1-screen.R

library(edwr)
library(dplyr)
library(tidyr)
library(lubridate)

data.raw <- "data/raw"

# patients eligible for inclusion
screen <- read_data(data.raw, "screen") %>%
    as.patients() %>%
    filter(discharge.datetime <= ymd("2016-06-30", tz = "US/Central"),
           age >= 18)

print(concat_encounters(screen$pie.id, 910))

# apply exclusion criteria

# randomly select 100 each before and after Oct 1, 2015 (ICD-10 implementation)
