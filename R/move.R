library(tidyverse)
library(data.table)
library(dtplyr)

distancing <- read_csv("data/distance/unfolded_covid _w social_distance _w weekly_patterns.csv")

fips <- c("26099", "12103", "55059", "42079", "55101", "39113") 

filtered_distancing <-
  distancing %>%
  filter(str_detect(county, glue_collapse(fips, sep = "|")))

ggplot(filtered_distancing,
       aes(datestr, sheltering_in_place_percent)) +
  geom_line() +
  facet_wrap(~ county_name)

ggplot(filtered_distancing,
       aes(datestr, sheltering_in_place_percent)) +
  geom_line(size = 0.74, colour = '#4DAF4A') +
  facet_wrap(~ county_name) +
  xlab("date") +
  ylab("percent of devices sheltering in place") +
  theme_hor() +
  ggsave("sheltering.png", height = 6, width = 10, dpi = 300)

travel <- 
  filtered_distancing %>%
  select(county_name, device_count, datestr, bucketed_distance_traveled_lt_1000:bucketed_distance_traveled_gt_50000) %>%
  pivot_longer(bucketed_distance_traveled_lt_1000:bucketed_distance_traveled_gt_50000, names_to = "distance") %>%
  mutate(distance = str_remove_all(distance, pattern = "bucketed_distance_traveled_"), 
         distance = str_remove_all(distance, pattern = "gt|lt"), 
         distance = str_replace_all(distance, pattern = "16001_50000", replacement = "16001-50000"),
         distance = str_replace_all(distance, pattern = "_50000", replacement = "50000+"),
         distance = str_replace_all(distance, pattern = "_1000", replacement = "-1000"),
         distance = str_replace_all(distance, pattern = "_", replacement = "-")) %>%
  mutate(distance = fct_relevel(distance, "50000+", after = 5))

options(scipen = 999)

ggplot(travel %>%
         filter(distance != "-1000") %>%
         group_by(county_name, datestr) %>%
         summarise(value = sum(value),
                   device_count = sum(device_count)),
       aes(datestr, value / device_count)) +
  geom_line(size = 0.74, colour = '#4DAF4A') +
  facet_wrap(~ county_name) +
  scale_colour_brewer(palette = 'Set1') +
  xlab("date") +
  ylab("percent of devices travelling between more than 1 km") +
  theme_hor() +
  ggsave("movement.png", height = 6, width = 10, dpi = 300)

unique(distancing$datestr)

travel %>%
  filter(distance != "-1000") %>%
  group_by(county_name, datestr) %>%
  summarise(value = sum(value),
            device_count = sum(device_count))


bind_rows(filtered_distancing %>%
            select(county_name, datestr, sheltering_in_place_percent) %>%
            rename(value = sheltering_in_place_percent) %>%
            mutate(variable = "Shelting in Place"),
          travel %>%
            filter(distance != "-1000") %>%
            group_by(county_name, datestr) %>%
            summarise(value = sum(value),
                      device_count = sum(device_count)) %>%
            mutate(value = value / device_count,
                   variable = "Moved > 1 km") %>%
            select(-device_count)) %>%
  ggplot(aes(datestr, value, colour = variable)) +
  geom_line(size = 0.74) +
  facet_wrap(~ county_name) +
  scale_colour_brewer(palette = 'Set1') +
  xlab("date") +
  ylab("% moving") +
  theme_hor() +
  ggsave("movement.png", height = 6, width = 10, dpi = 300)
          


