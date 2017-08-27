library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(tools)

dirty <- 'signif.txt'

eq_clean_data <- function(x) {
  data <- read_tsv(x, col_names = TRUE, col_types = NULL) %>%
    unite(date, 'YEAR', 'MONTH', 'DAY') %>%
    mutate(date = ymd(date)) %>%
    mutate(LATITUDE = as.numeric('LATITUDE')) %>%
    mutate(LONGITUDE = as.numeric('LONGITUDE'))
  return(data)
}

eq_loc_helper <- function(x) {
  gsub(".*:", "", x)
}

eq_location_clean <- function(x) {
  data <- x %>%
  mutate(LOCATION_NAME = eq_loc_helper(LOCATION_NAME)) %>%
    mutate(LOCATION_NAME = tolower(LOCATION_NAME)) %>%
    mutate(LOCATION_NAME = toTitleCase(LOCATION_NAME))
  return(data)
  }
