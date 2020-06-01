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


