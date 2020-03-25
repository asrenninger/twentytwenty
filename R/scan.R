library(tidyverse)
library(janitor)
library(lubridate)

##

read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
  clean_names() %>%
  filter(country_region == "US") %>%
  gather(date, cases, x1_22_20:x3_20_20) %>%
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
  gather(date, cases, x1_22_20:x3_20_20) %>%
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
  gather(date, cases, x1_22_20:x3_18_20) %>%
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

pal <- colorNumeric("viridis", NULL)

read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports/03-23-2020.csv") %>%
  clean_names() %>%
  filter(country_region == "US") %>%
  group_by(province_state, long, lat) %>%
  summarise(cases = sum(confirmed),
            deaths = sum(deaths)) %>% 
  filter(cases > 0) %>%
  leaflet() %>% 
  addTiles() %>%
  addCircles(~long, ~lat, popup = ~as.character(cases), radius = ~sqrt(cases), color = ~pal(cases), label = ~as.character(cases))

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

##

covid <- 
  read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports/03-23-2020.csv") %>%
  clean_names() %>%
  filter(country_region == "US") %>%
  group_by(province_state, long, lat) %>%
  summarise(cases = sum(confirmed),
            deaths = sum(deaths)) %>% 
  filter(cases > 0) %>%
  st_as_sf(coords = c("long", "lat"), remove = FALSE) %>%
  filter(!province_state %in% states$name) %>%
  group_by(province_state, long, lat) %>%
  summarise(cases = sum(cases))


