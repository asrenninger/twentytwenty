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

pal <- colorQuantile("viridis", NULL, n = 3)

read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports/03-23-2020.csv") %>%
  clean_names() %>%
  filter(country_region == "US") %>%
  group_by(province_state, long, lat) %>%
  summarise(cases = sum(confirmed),
            deaths = sum(deaths)) %>% 
  filter(cases > 0) %>%
  leaflet() %>% 
  addTiles() %>%
  addCircles(~long, ~lat, popup = ~as.character(cases), radius = ~sqrt(cases * 100), color = ~pal(cases), label = ~as.character(cases))

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

##

read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, coefficient)) +
  geom_bar(stat = 'identity')

metro <- c("Bucks", "Chester", "Delaware", "Montgomery", "Philadelphia", "Burlington", "Camden", "Gloucester", "New Castle")
  
p1 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "New Jersey" | state == "Pennsylvania" | state == "Delaware") %>%
  filter(county %in% metro) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(multiplier = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, multiplier)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  theme_bw()

p2 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "New Jersey" | state == "Pennsylvania" | state == "Delaware") %>%
  filter(county %in% metro) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, change)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  theme_bw()

p3 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "New Jersey" | state == "Pennsylvania" | state == "Delaware") %>%
  filter(county %in% metro) %>%
  group_by(date, county) %>%
  summarise(cases = sum(cases)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, cases, fill = county)) +
  geom_bar(stat = 'identity', position = 'stack') +
  theme_bw()

library(patchwork)

p <- p1 + p2 + p3

ggsave("region.png", height = 6, width = 18, dpi = 300)

##

p1 <- read_csv("~/Downloads/multiTimeline.csv", skip = 1) %>%
  gather(Term, Searches, 2:4) %>%
  mutate(Term = str_remove_all(Term, ": \\(United States\\)")) %>%
  ggplot(aes(Day, Searches, color = Term)) +
  geom_line(size = 2) +
  ylab("Searches (% total)") +
  scale_color_brewer(palette = 'Set1') +
  theme_hor() +
  ggsave("searches.png", height = 5, width = 8, dpi = 300)

p2 <- read_csv("~/Downloads/covid-19-wuhan-virus-chinese-v-stories-over-time-20200331151417.csv") %>%
  select(date, contains("ratio")) %>%
  gather(Term, Searches, 2:4) %>%
  mutate(Term = str_remove_all(Term, "-ratio")) %>%
  ggplot(aes(date, Searches, color = Term)) +
  geom_line(size = 2) +
  ylab("Articles (% total)") +
  xlab("Day") +
  scale_color_brewer(palette = 'Set1') +
  theme_hor() +
  ggsave("articles.png", height = 5, width = 8, dpi = 300)

p <- p1 + p2 + plot_layout(guides = 'collect') + plot_annotation(caption = "Source: Google, Media Cloud")

ggsave("coverage.png", height = 5, width = 16, dpi = 300)

##

library(readxl)
library(janitor)
library(lubridate)
library(geofacet)

claims <- 
  read_xlsx("data/claims.xlsx") %>%
  clean_names() %>%
  drop_na(state) %>%
  filter(!str_detect(state, "Run Date:|Puerto|Virgin"))

ggplot(rename(claims, name = state), aes(filed_week_ended, initial_claims)) +
  geom_line(colour = brewer.pal(9, "Set1")[3]) +
  facet_geo(~ name, grid ="us_state_grid1") +
  xlab("week") +
  ylab("filings for unemployment insurance") +
  theme_hor() + 
  ggsave("claims.png", height = 10, width = 16, dpi = 300)


