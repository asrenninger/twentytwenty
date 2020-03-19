library(tidyverse)
library(janitor)
library(lubridate)

##

read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
  clean_names() %>%
  filter(country_region == "US") %>%
  gather(date, cases, x1_22_20:x3_16_20) %>%
  mutate(date = str_remove_all(date, "x")) %>%
  mutate(date = str_replace_all(date, "_", "-")) %>%
  mutate(date = mdy(date)) %>%
  filter(date > as_date('2020-02-20')) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(date, cases)) +
  geom_point() +
  geom_smooth()

read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
  clean_names() %>%
  filter(country_region == "US") %>%
  gather(date, cases, x1_22_20:x3_16_20) %>%
  mutate(date = str_remove_all(date, "x")) %>%
  mutate(date = str_replace_all(date, "_", "-")) %>%
  mutate(date = mdy(date)) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date))

library(tigris)

states <- states(class = 'sf') %>%
  clean_names()

library(geofacet)

read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
  clean_names() %>%
  filter(country_region == "US") %>%
  gather(date, cases, x1_22_20:x3_16_20) %>%
  mutate(date = str_remove_all(date, "x")) %>%
  mutate(date = str_replace_all(date, "_", "-")) %>%
  mutate(date = mdy(date)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4269) %>%
  st_join(states) %>%
  st_drop_geometry() %>%
  group_by(date, name) %>%
  summarise(cases = sum(cases)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  filter(cases > 0) %>%
  ggplot(aes(date, coefficient)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ name, scales = 'free')

read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
  clean_names() %>%
  filter(country_region == "US") %>%
  gather(date, cases, x1_22_20:x3_16_20) %>%
  mutate(date = str_remove_all(date, "x")) %>%
  mutate(date = str_replace_all(date, "_", "-")) %>%
  mutate(date = mdy(date)) %>%
  filter(date > as_date("2020-02-20")) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(date, cases)) +
  geom_bar(stat = 'identity')

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
  filter(!province_state %in% states$name) %>%
  group_by(province_state) %>%
  summarise(cases = sum(cases)) %>%
  mapview::mapview(zcol = "cases")

##

library(leaflet)

##

read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
  clean_names() %>%
  filter(country_region == "US") %>%
  gather(date, cases, x1_22_20:x3_17_20) %>%
  mutate(date = str_remove_all(date, "x")) %>%
  mutate(date = str_replace_all(date, "_", "-")) %>%
  mutate(date = mdy(date)) %>%
  st_as_sf(coords = c("long", "lat"), remove = FALSE) %>%
  filter(!province_state %in% states$name) %>%
  group_by(province_state, long, lat) %>%
  summarise(cases = sum(cases)) %>% 
  leaflet() %>% 
  addTiles() %>%
  addCircles(~long, ~lat, popup = ~as.character(cases), radius = ~cases *100, fill = ~cases, label = ~as.character(cases))

##

read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv") %>%
  clean_names() %>%
  filter(country_region == "US") %>%
  gather(date, cases, x1_22_20:x3_15_20) %>%
  mutate(date = str_remove_all(date, "x")) %>%
  mutate(date = str_replace_all(date, "_", "-")) %>%
  mutate(date = mdy(date)) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  arrange(desc(date))


