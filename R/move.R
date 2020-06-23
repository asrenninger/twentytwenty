library(tidyverse)
library(glue)
library(data.table)
library(dtplyr)

distancing <- read_csv("data/distance/unfolded_covid _w social_distance _w weekly_patterns.csv")

fips <- c("26099", "12103", "55059", "42079", "55101", "39113") 

filtered_distancing <-
  distancing %>%
  filter(str_detect(county, glue_collapse(fips, sep = "|"))) %>%
  filter(device_count != 0)

ggplot(filtered_distancing,
       aes(datestr, sheltering_in_place_percent)) +
  geom_line() +
  facet_wrap(~ county_name)

ggplot(filtered_distancing,
       aes(datestr, sheltering_in_place_percent)) +
  geom_smooth(size = 0.74, colour = '#4DAF4A', se = FALSE) +
  geom_point(alpha = 0.5, colour = '#4DAF4A') +
  facet_wrap(~ county_name) +
  xlab("date") +
  ylab("percent of devices sheltering in place") +
  theme_hor() +
  theme(legend.position = 'bottom') +
  ggsave("sheltering.png", height = 6, width = 10, dpi = 300)

travel <- 
  filtered_distancing %>%
  select(county_name, device_count, datestr, bucketed_distance_traveled_lt_1000:bucketed_distance_traveled_gt_50000) %>%
  pivot_longer(bucketed_distance_traveled_lt_1000:bucketed_distance_traveled_gt_50000, names_to = "distance") %>%
  mutate(distance = str_remove_all(distance, pattern = "bucketed_distance_traveled_"), 
         distance = str_remove_all(distance, pattern = "gt|lt"), 
         distance = str_replace_all(distance, pattern = "1001_2000", replacement = "1-2"),
         distance = str_replace_all(distance, pattern = "2001_8000", replacement = "2-8"),
         distance = str_replace_all(distance, pattern = "8001_16000", replacement = "8-16"),
         distance = str_replace_all(distance, pattern = "16001_50000", replacement = "16-50"),
         distance = str_replace_all(distance, pattern = "_50000", replacement = "50+"),
         distance = str_replace_all(distance, pattern = "_1000", replacement = "-1 (km)"),
         distance = str_replace_all(distance, pattern = "_", replacement = "-")) %>%
  mutate(distance = fct_relevel(distance, levels = c("-1 (km)", "1-2", "2-8", "8-16", "16-50", "50+")))

options(scipen = 999)

ggplot(travel %>%
         filter(distance != "-1 (km)") %>%
         group_by(county_name, datestr),
       aes(datestr, value / device_count, colour = distance)) +
  geom_smooth(size = 0.74, se = FALSE) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ county_name) +
  scale_colour_brewer(palette = 'Set1', name = "distance (km)") +
  xlab("date") +
  ylab("percent of devices travelling more than 1 km") +
  theme_hor() +
  theme(legend.position = 'bottom') +
  ggsave("movement.png", height = 6, width = 10, dpi = 300)

visits <- 
  filtered_distancing %>%
  select(county_name, device_count, datestr, visits_by_day_Construction:visits_by_day_Health_Care_and_Social_Assistance) %>%
  pivot_longer(visits_by_day_Construction:visits_by_day_Health_Care_and_Social_Assistance, names_to = "visits") %>%
  mutate(visits = str_remove_all(visits, pattern = "visits_by_day_"),
         visits = str_remove_all(visits, pattern = "_.*"), 
         visits = str_replace_all(visits, pattern = "Real", replacement = "Real Estate")) %>%
  mutate(visits = case_when(visits == "Management" ~ "Professional",
                            visits == "Administrative" ~ "Professional",
                            visits == "Finance" ~ "Professional",
                            TRUE ~ visits)) %>%
  group_by(datestr, county_name, device_count, visits) %>%
  summarise(value = sum(value)) %>%
  ungroup()

ggplot(visits %>%
         filter(visits != "Transportation" & visits != "Information" & visits != "Real Estate" & visits != "Wholesale"
                & visits != "Construction" & visits != "Educational") %>%
         filter(value != 0) %>%
         mutate(visits = fct_relevel(visits, levels = c("Retail", "Professional", "Heatlh", "Construction", "Manufacturing", "Educational"))) %>%
         group_by(county_name, visits) %>%
         mutate(value = scale(value)), 
       aes(datestr, value / device_count, colour = visits)) +
  geom_smooth(size = 0.74, se = FALSE) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ county_name) +
  scale_colour_brewer(palette = 'Set1', name = "industry") +
  xlab("date") +
  ylab("visits per industry (% devices)") +
  theme_hor() +
  theme(legend.position = 'bottom') +
  ggsave("visits.png", height = 6, width = 10, dpi = 300)






          


