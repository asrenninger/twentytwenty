library(tidyverse)
library(haven)
library(lubridate)
library(janitor)

one <- read_sav("data/appc/APPC Wave 1_Client_Final 2020-06-19_with weights.sav") %>%
  as_tibble() %>%
  clean_names() 

two <- read_sav("data/appc/APPC Wave 2_Client_Prelim 2020-06-22 1200.sav") %>%
  as_tibble() %>%
  clean_names()

glimpse(one)
glimpse(two)

re_date <- function(x) { as_date(x, format = '%m/%d/%Y', tz = 'UTC') }
re_time <- function(x) { hms(x) }

one_cleaned <- 
  one %>%
  mutate_if(str_detect(colnames(.), "date"), re_date) %>%
  mutate_if(str_detect(colnames(.), "time"), re_time)

two_cleaned <- 
  two %>%
  mutate_if(str_detect(colnames(.), "date"), re_date) %>%
  mutate_if(str_detect(colnames(.), "time"), re_time)

library(tmap)
library(tmaptools)

tmap_mode("view")

tm_shape(one_cleaned %>%
           st_as_sf(coords = c("pabs_longitude", "pabs_latitude"))) +
  tm_dots(col = 'red') +
  tm_shape(two_cleaned %>%
             st_as_sf(coords = c("pabs_longitude", "pabs_latitude"))) +
  tm_dots(col = 'blue')




