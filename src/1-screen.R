# 1-screen.R

library(edwr)
library(dplyr)
library(tidyr)
library(lubridate)

data.raw <- "data/raw"

screen <- read_data(data.raw, "screen") %>%
    as.patients() %>%
    filter(discharge.datetime <= ymd("2016-06-30", tz = "US/Central"))

# randomly select 100 each before and after Oct 1, 2015 (ICD-10 implementation)
