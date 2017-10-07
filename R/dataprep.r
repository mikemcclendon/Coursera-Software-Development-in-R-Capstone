library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(tools)

dirty <- 'signif.txt'

#' Create a cleaned version of the NOAA dataset
#'
#' This function cleans and readies the U.S. National Oceanographic
#' and Atmospheric Administration (NOAA) dataset on significant earthquakes
#' around the world
#'
#' https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1
#'
#' @param x the raw NOAA data to be cleaned
#'
#' @return data.frame containing the cleaned NOAA data
#'
#' @examples \dontrun{eq_clean_data(dirty)}
#'
#' @export
eq_clean_data <- function(x) {
  data <- read_tsv(x, col_names = TRUE, col_types = NULL) %>%
    unite(date, 'YEAR', 'MONTH', 'DAY') %>%
    mutate(date = ymd(date)) %>%
    mutate(LATITUDE = as.numeric('LATITUDE')) %>%
    mutate(LONGITUDE = as.numeric('LONGITUDE'))
  return(data)
}

#' Helper function to strip colons out of country name
#'
#' @param x LOCATION_NAME field or any string requiring removal
#' of colons
#'
#' @return string with colons stripped out
#'
#' @examples \dontrun{eq_loc_helper("wo:rd")}
#' @export
eq_loc_helper <- function(x) {
  gsub(".*:", "", x)
}

#' Function to clean the LOCATION_NAME field in the NOAA
#' earthquake dataset
#'
#' @param x NOAA signification earthquake dataset
#'
#' @return data.frame with cleaned LOCATION_NAME field
#'
#' @examples \dontrun{eq_location_clean(dirty)}
#' @export

eq_location_clean <- function(x) {
  data <- x %>%
  mutate(LOCATION_NAME = eq_loc_helper(LOCATION_NAME)) %>%
    mutate(LOCATION_NAME = tolower(LOCATION_NAME)) %>%
    mutate(LOCATION_NAME = toTitleCase(LOCATION_NAME))
  return(data)
  }
