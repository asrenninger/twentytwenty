library(tidyverse)
library(dtplyr)
library(data.table)

##

library(fs)
library(glue)

##

files <- dir_ls("data/poi")
files <- files[str_detect(files, "core_poi")]

##

poi <- map_df(files, read_csv)

##

poi <- lazy_dt(poi)

phl <- 
  poi %>% filter(region == "PA" & city == "Philadelphia") %>%
  as_tibble()

glimpse(phl)

##

files <- dir_ls("data/safegraph/v1/main-file")

patterns <- 
  reduce(
    map(files[1:5], function(x) {
      read_csv(x) %>%
        filter(safegraph_place_id %in% phl$safegraph_place_id)
    }),
    rbind
  )

##

glimpse(patterns)

##

library(lubridate)

##

march <- filter(patterns, date_range_end < as_date("2020-04-01"))

ordest <- 
  march %>%
  select(safegraph_place_id, visitor_home_cbgs) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, "\\{|\\}"),
         visitor_home_cbgs = str_remove_all(visitor_home_cbgs, "\"|")) %>%
  filter(visitor_home_cbgs != "") %>%
  separate_rows(visitor_home_cbgs, sep = ",") %>%
  separate(visitor_home_cbgs, into = c("cbgs", "visits")) %>%
  mutate(visits = as.numeric(visits))
  


