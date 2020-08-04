library(tidyverse)
library(magrittr)
library(jsonlite)

test <- 
  fromJSON("https://policies.equityschool.plus/reopening-observations.json") %>%
  use_series(states) %>%
  rename(state = name) %>%
  unnest(observations) %>%
  unnest_longer(categories) %>%
  mutate(categories = categories$name) %>%
  as_tibble() %>%
  write_csv("school_reopening_redux.csv")

test %>%
  group_by(categories) %>%
  summarise(n = n()) %>%
  write_csv("top_line.csv")
