library(tidyverse)
library(labelled)
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

two_cleaned <- 
  two %>%
  mutate_if(str_detect(colnames(.), "date"), re_date) %>%
  mutate_if(str_detect(colnames(.), "time"), re_time)

one_cleaned <- 
  one %>%
  filter(panelistid %in% two_cleaned$panelistid) %>%
  mutate_if(str_detect(colnames(.), "date"), re_date) %>%
  mutate_if(str_detect(colnames(.), "time"), re_time)


library(tmap)
library(tmaptools)
library(sf)

tmap_mode("view")

tm_shape(one_cleaned %>%
           filter(panelistid %in% one_cleaned$panelistid) %>%
           st_as_sf(coords = c("pabs_longitude", "pabs_latitude"))) +
  tm_dots(col = "panelistid") +
  tm_shape(two_cleaned %>%
             st_as_sf(coords = c("pabs_longitude", "pabs_latitude"))) +
  tm_dots(col = "panelistid")

sum(two_cleaned$panelistid %in% one_cleaned$panelistid)

glimpse(one_cleaned)
glimpse(two_cleaned)

one_basics <- select(one_cleaned, panelistid, pabs_latitude, pabs_longitude, pcensus_block_group, pmstate, start_date, end_date)
two_basics <- select(two_cleaned, panelistid, pabs_latitude, pabs_longitude, pcensus_block_group, pmstate, start_date, end_date)


one_sub <-
  one_cleaned %>%
  select_if(str_detect(colnames(.), "emp")) %>%
  mutate(panelistid = one_cleaned$panelistid) %>%
  left_join(one_basics)

two_sub <-
  two_cleaned %>%
  select_if(str_detect(colnames(.), "emp")) %>%
  mutate(panelistid = two_cleaned$panelistid) %>%
  left_join(two_basics)

combined <- bind_rows(one_sub %>%
                        mutate(wave = "1"), 
                      two_sub %>%
                        mutate(wave = "2"))

ggplot(combined %>%
         group_by(start_date) %>%
         summarise(lo_temp = sum(employ_3),
                   lo_full = sum(employ_4)) %>%
         pivot_longer(lo_temp:lo_full),
       aes(start_date, value)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ name, ncol = 1)

variables <- "employ_1 employ_2 employ_3 employ_4 employ_5 employ_7"
variables <- str_split(variables, pattern = " ")[[1]]

test <- 
  left_join(one_sub %>%
              select(panelistid, variables) %>%
              rename_at(vars(employ_1:employ_7), function(x) paste(x, "w1", sep = "_")),
            two_sub %>%
              select(panelistid, variables) %>%
              rename_at(vars(employ_1:employ_7), function(x) paste(x, "w2", sep = "_"))) %>%
  mutate(change_1 = employ_1_w1 != employ_1_w2,
         change_2 = employ_2_w1 != employ_2_w2,
         change_3 = employ_3_w1 != employ_3_w2,
         change_4 = employ_4_w1 != employ_4_w2,
         change_5 = employ_5_w1 != employ_5_w2,
         change_7 = employ_7_w1 != employ_7_w2) %>%
  select(panelistid, change_1:change_7)








