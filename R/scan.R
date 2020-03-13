library(tidyverse)
library(janitor)
library(lubridate)

##

read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
  clean_names() %>%
  filter(country_region == "US") %>%
  gather(date, cases, x1_22_20:x3_10_20) %>%
  mutate(date = str_remove_all(date, "x")) %>%
  mutate(date = str_replace_all(date, "_", "-")) %>%
  mutate(date = mdy(date)) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(date, cases)) +
  geom_point()

##

library(sf)

##

read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
  clean_names() %>%
  filter(country_region == "US") %>%
  gather(date, cases, x1_22_20:x3_10_20) %>%
  mutate(date = str_remove_all(date, "x")) %>%
  mutate(date = str_replace_all(date, "_", "-")) %>%
  mutate(date = mdy(date)) %>%
  st_as_sf(coords = c("long", "lat")) %>%
  group_by(province_state) %>%
  summarise(cases = sum(cases)) %>%
  mapview::mapview(zcol = "cases")

##

library(leaflet)

##

read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
  clean_names() %>%
  filter(country_region == "US") %>%
  gather(date, cases, x1_22_20:x3_10_20) %>%
  mutate(date = str_remove_all(date, "x")) %>%
  mutate(date = str_replace_all(date, "_", "-")) %>%
  mutate(date = mdy(date)) %>%
  st_as_sf(coords = c("long", "lat"), remove = FALSE) %>%
  group_by(province_state, long, lat) %>%
  summarise(cases = sum(cases)) %>% 
  leaflet() %>% 
  addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(cases), label = ~as.character(cases))

