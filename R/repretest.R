library(tidyverse)
library(janitor)

##

read_csv("data/2020APPCPreTestII-3-6-20_March 23, 2020_11.36.csv") %>% names()

pretest <- 
  read_csv("data/2020APPCPreTestII-3-6-20_March 23, 2020_11.36.csv") %>%
  clean_names()

glimpse(pretest)  

##

library(tigris)
library(sf)

##

counties <- counties(cb = TRUE, class = 'sf') %>%
  filter(!str_detect(STATEFP, "15|02|60|66|69|72|78")) %>%
  st_transform(102003)

states <- states(cb = TRUE, class = 'sf') %>%
  filter(!str_detect(GEOID, "15|02|60|66|69|72|78")) %>%
  st_transform(102003)

##

library(RColorBrewer)

##

ggplot() +
  geom_sf(data = states, aes()) +
  geom_sf(data = pretest %>%
            drop_na(location_longitude, location_latitude) %>%
            st_as_sf(coords = c("location_longitude", "location_latitude"), remove = FALSE, crs = 4326) %>%
            st_transform(102003) %>%
            st_intersection(st_as_sfc(st_bbox(states))) %>%
            mutate(vote = case_when(votechoice == "I am almost certain to vote FOR someone other than Donald Trump." ~ "Against Trump",
                                    votechoice == "I am almost certain to vote FOR Donald Trump no matter whom the Democrats nominate for president or who runs as a third-party candidate." ~ "For Trump",
                                    votechoice == "My decision for whom to vote will be determined by who is nominated by the Democrats or who runs as a third-party candidate." ~ "Undecided",
                                    TRUE ~ votechoice)), 
          aes(colour = covid_concern1, fill = covid_concern1)) +
  scale_color_brewer(palette = 'Set1', name = "Likelihood\nof contracting") +
  scale_fill_brewer(palette = 'Set1', name = "Likelihood\nof contracting") +
  theme_map() +
  ggsave("concern_second.png", height = 8, width = 15, dpi = 300)

##

us <- unique(fips_codes$state)[1:51]

##

tracts <- reduce(
  map(us, function(x) {
    tracts(x) %>%
      st_as_sf()
  }), 
  rbind
)

##

library(tidycensus)

##

gini_us <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B19083_001", state = x) %>% 
    select(GEOID, estimate) %>%
    rename(GINI = estimate)
})

race_us <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = c("B02001_003E", "B02001_002E"), state = x,
          output = "wide") %>%
    rename(black = B02001_003E,
           white = B02001_002E) %>%
    select(GEOID, black, white)
})

foreigners_us <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = c("B05006_124", "B05006_001"), state = x,
          output = "wide") %>%
    rename(latino = B05006_124E,
           foreign = B05006_001E) %>%
    select(GEOID, latino, foreign)
})

mobility_us <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = c("B07001_033", "B07001_081"), state = x,
          output = "wide") %>%
    rename(moved_nation = B07001_033E,
           moved_abroad = B07001_081E) %>%
    select(GEOID, moved_nation, moved_abroad)
})

population_us <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = c("B01001_001", "B11016_001", "B01002_001E", "B06009_005E"), state = x,
          output = "wide") %>%
    rename(population = B01001_001E,
           households = B11016_001E,
           median_age = B01002_001E,
           higher_edu = B06009_005E) %>%
    select(GEOID, population, households, median_age, higher_edu)
})

income_us <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = c("B06011_001E", "B25113_001E"), state = x,
          output = "wide") %>%
    rename(median_income = B06011_001E,
           median_rent = B25113_001E) %>%
    select(GEOID, median_income, median_rent)
})

tenure_us <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = c("B25003_001E", "B25003_002E", "B25003_003E"), state = x,
          output = "wide") %>%
    rename(units = B25003_001E,
           owned = B25003_002E,
           rented = B25003_003E) %>%
    select(GEOID, units, owned, rented)
})

employment_us <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = c("C18120_002E", "C18120_006E", "C18120_003E"), state = x,
          output = "wide") %>%
    rename(labor_force = C18120_002E,
           unemployed = C18120_006E,
           employed = C18120_003E) %>%
    select(GEOID, labor_force, unemployed, employed)
})

comparison <- map_df(us, function(x){ 
  get_acs(geography = "tract", variables = c("C18120_002E", "C18120_006E", "C18120_003E", "B06011_001E"), state = x,
          output = "wide", year = 2015) %>%
    rename(labor_force = C18120_002E,
           unemployed = C18120_006E,
           employed = C18120_003E,
           median_income = B06011_001E) %>%
    select(GEOID, labor_force, unemployed, employed, median_income)
})

##

library(tictoc)

##

tracts <- 
  read_sf("data/tracts.shp") %>%
  filter(!str_detect(STATEFP, "15|02|60|66|69|72|78")) %>%
  transmute(GEOID = GEOID, STATEFP = STATEFP, COUNTYFP = COUNTYFP, area = ALAND) %>%
  st_transform(102003)

contiguous <- 
  tracts %>%
  filter(!str_detect(STATEFP, "15|02|60|66|69|72|78")) %>%
  transmute(GEOID = GEOID, STATEFP = STATEFP, COUNTYFP = COUNTYFP, area = ALAND) %>%
  st_transform(102003)

##

census <-
  population_us %>%
  left_join(race_us) %>%
  left_join(mobility_us) %>%
  left_join(foreigners_us) %>%
  left_join(gini_us) %>%
  left_join(income_us) %>%
  left_join(tenure_us) %>%
  left_join(employment_us) %>%
  left_join(contiguous) %>%
  drop_na(population) %>%
  st_as_sf() %>%
  mutate(pct_latino = latino / (population + 1),
         pct_foreign = foreign / (population + 1),
         pct_black = black / (population + 1),
         pct_white = white / (population + 1),
         density = as.numeric(area) / (population + 1),
         churn = moved_nation + moved_abroad / (population + 1),
         unemp_rate = unemployed / labor_force) %>%
  select(GEOID, STATEFP, COUNTYFP, everything())

##

geolocated <-
  pretest %>%
  drop_na(location_longitude, location_latitude) %>%
  st_as_sf(coords = c("location_longitude", "location_latitude"), remove = FALSE, crs = 4326) %>%
  st_transform(102003) %>%
  st_intersection(st_as_sfc(st_bbox(states))) %>%
  mutate(vote = case_when(votechoice == "I am almost certain to vote FOR someone other than Donald Trump." ~ "Against Trump",
                          votechoice == "I am almost certain to vote FOR Donald Trump no matter whom the Democrats nominate for president or who runs as a third-party candidate." ~ "For Trump",
                          votechoice == "My decision for whom to vote will be determined by who is nominated by the Democrats or who runs as a third-party candidate." ~ "Undecided",
                          TRUE ~ votechoice))

##

community <-
  geolocated %>%
  st_join(census)

##

glimpse(community)

##

ggplot(community %>%
         st_drop_geometry() %>%
         drop_na(vote) %>%
         select(vote, unemp_rate, churn, density, pct_black, median_income, median_age) %>%
         mutate(density = log(density)) %>%
         gather(variable, value, unemp_rate:median_age) %>%
         mutate(variable = str_replace_all(variable, "_", " ")) %>%
         group_by(variable) %>%
         mutate(pct = ntile(value, 100)) %>%
         ungroup() %>%
         filter(pct != 1 & pct != 100)) +
  geom_jitter(aes(vote, value, colour = vote)) +
  stat_summary(aes(vote, value), fun.y = mean, geom = "point", size = 3) +
  facet_wrap(~ variable, scales = 'free') +
  scale_color_brewer(palette = 'Set1') +
  coord_flip() + 
  theme_rot() +
  ggsave("area_second.png", height = 8, width = 15, dpi = 300)

##

companies <- 
  read_csv("data/russell_geocoded.csv") %>%
  drop_na(lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326) %>%
  st_transform(102003) %>%
  st_intersection(states)

##

coords_companies <-
  companies %>%
  st_coordinates() %>%
  as_tibble()

coords_respondents <- 
  community %>%
  st_coordinates() %>%
  as_tibble()

##

library(spdep)
library(FNN)

##

nn <- get.knnx(coords_companies, coords_respondents, k = 1)

distances <-
  as.data.frame(nn$nn.dist) %>%
  rownames_to_column(var = "respondents") %>%
  gather(companies, dist_companies, V1) %>%
  arrange(as.numeric(respondents)) %>%
  group_by(respondents) %>%
  summarise(dist_listed_company = mean(dist_companies)) %>%
  arrange(as.numeric(respondents)) %>% 
  select(-respondents) %>%
  bind_cols(community) %>%
  select(response_id, dist_listed_company)

##

ggplot(community %>%
         st_drop_geometry() %>%
         drop_na(vote) %>%
         left_join(distances) %>%
         mutate(pct = ntile(dist_listed_company, 100)) %>%
         filter(pct != 1 & pct < 90) %>%
         rename(`dist listed companie` = dist_listed_company)) +
  geom_jitter(aes(vote, `dist listed companie`, colour = vote)) +
  stat_summary(aes(vote, `dist listed companie`), fun.y = mean, geom = "point", size = 3) +
  scale_color_brewer(palette = 'Set1') +
  coord_flip() + 
  theme_rot() + 
  ggsave("distance_second.png", height = 4, width = 6, dpi = 300)

##

restaurants <- 
  read_sf("data/restaurants.geojson", crs = 4326) %>%
  st_transform(102003) %>%
  st_intersection(states) %>%
  st_coordinates() %>%
  as_tibble()

coords_respondents <- 
  community %>%
  st_coordinates() %>%
  as_tibble()

##

library(spdep)
library(FNN)

##

nn <- get.knnx(restaurants, coords_respondents, k = 3)

distances <-
  as.data.frame(nn$nn.dist) %>%
  rownames_to_column(var = "respondents") %>%
  gather(restaurants, dist_restaurant, V1:V3) %>%
  arrange(as.numeric(respondents)) %>%
  group_by(respondents) %>%
  summarise(dist_restaurant = mean(dist_restaurant)) %>%
  arrange(as.numeric(respondents)) %>% 
  select(-respondents) %>%
  bind_cols(community) %>%
  select(response_id, dist_restaurant)

##

p1 <- 
  ggplot(community %>%
           st_drop_geometry() %>%
           drop_na(vote) %>%
           left_join(distances) %>%
           mutate(pct = ntile(dist_restaurant, 100)) %>%
           filter(pct != 1 & pct != 100) %>%
           rename(`dist restaurant (m)` = dist_restaurant)) +
  geom_jitter(aes(vote, `dist restaurant (m)`, colour = vote)) +
  stat_summary(aes(vote, `dist restaurant (m)`), fun.y = mean, geom = "point", size = 3) +
  scale_color_brewer(palette = 'Set1') +
  coord_flip() + 
  theme_rot() + 
  ggsave("restaurants.png", height = 4, width = 6, dpi = 300)

##

stations <- 
  read_sf("data/stations.geojson", crs = 4326) %>%
  st_transform(102003) %>%
  st_intersection(states) %>%
  st_coordinates() %>%
  as_tibble()

coords_respondents <- 
  community %>%
  st_coordinates() %>%
  as_tibble()

##

library(spdep)
library(FNN)

##

nn <- get.knnx(stations, coords_respondents, k = 3)

distances <-
  as.data.frame(nn$nn.dist) %>%
  rownames_to_column(var = "respondents") %>%
  gather(stations, dist_station, V1:V3) %>%
  arrange(as.numeric(respondents)) %>%
  group_by(respondents) %>%
  summarise(dist_station = mean(dist_station)) %>%
  arrange(as.numeric(respondents)) %>% 
  select(-respondents) %>%
  bind_cols(community) %>%
  select(response_id, dist_station)

##

p2 <- 
  ggplot(community %>%
           st_drop_geometry() %>%
           drop_na(vote) %>%
           left_join(distances) %>%
           mutate(pct = ntile(dist_station, 100)) %>%
           filter(pct != 1 & pct != 100) %>%
           rename(`dist gas station (m)` = dist_station)) +
  geom_jitter(aes(vote, `dist gas station (m)`, colour = vote)) +
  stat_summary(aes(vote, `dist gas station (m)`), fun.y = mean, geom = "point", size = 3) +
  scale_color_brewer(palette = 'Set1') +
  coord_flip() + 
  theme_rot() + 
  ggsave("stations.png", height = 4, width = 6, dpi = 300)

##

library(patchwork)

##

p1 + p2 + plot_layout(guides = 'collect')
ggsave("distances.png", height = 8, width = 18, dpi = 300)

##

bars <- 
  read_sf("data/bars.geojson", crs = 4326) %>%
  st_transform(102003) %>%
  st_intersection(states) %>%
  st_coordinates() %>%
  as_tibble()

coords_respondents <- 
  community %>%
  st_coordinates() %>%
  as_tibble()

##

library(spdep)
library(FNN)

##

nn <- get.knnx(stations, coords_respondents, k = 3)

distances <-
  as.data.frame(nn$nn.dist) %>%
  rownames_to_column(var = "respondents") %>%
  gather(bars, dist_bar, V1:V3) %>%
  arrange(as.numeric(respondents)) %>%
  group_by(respondents) %>%
  summarise(dist_bar = mean(dist_bar)) %>%
  arrange(as.numeric(respondents)) %>% 
  select(-respondents) %>%
  bind_cols(community) %>%
  select(response_id, dist_bar)

##

p3 <- 
  ggplot(community %>%
           st_drop_geometry() %>%
           drop_na(vote) %>%
           left_join(distances) %>%
           mutate(pct = ntile(dist_bar, 100)) %>%
           filter(pct != 1 & pct != 100) %>%
           rename(`dist bar (m)` = dist_bar)) +
  geom_jitter(aes(vote, `dist bar (m)`, colour = vote)) +
  stat_summary(aes(vote, `dist bar (m)`), fun.y = mean, geom = "point", size = 3) +
  scale_color_brewer(palette = 'Set1') +
  coord_flip() + 
  theme_rot() + 
  ggsave("bars.png", height = 4, width = 8, dpi = 300)

read_csv("data/2020APPCPreTestII-3-6-20_March 23, 2020_11.36.csv") %>%
  clean_names() %>%
  names()

##

covid <- 
  read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports/03-23-2020.csv") %>%
  clean_names() %>%
  filter(country_region == "US") %>%
  group_by(province_state, long, lat) %>%
  summarise(cases = sum(confirmed),
            deaths = sum(deaths)) %>% 
  filter(cases > 0) %>%
  st_as_sf(coords = c("long", "lat"), remove = FALSE, crs = 4326) %>%
  st_transform(102003) %>%
  filter(!province_state %in% states$name) %>%
  group_by(province_state, long, lat) %>%
  summarise(cases = sum(cases),
            deaths = sum(deaths))

##

deaths <- 
  covid %>%
  filter(deaths > 0) %>%
  st_intersection(states) %>%
  st_coordinates() %>%
  as_tibble()

coords_respondents <- 
  community %>%
  st_coordinates() %>%
  as_tibble()

##

library(spdep)
library(FNN)

##

nn <- get.knnx(deaths, coords_respondents, k = 1)

distances <-
  as.data.frame(nn$nn.dist) %>%
  rownames_to_column(var = "respondents") %>%
  gather(deaths, dist_death, V1) %>%
  arrange(as.numeric(respondents)) %>%
  group_by(respondents) %>%
  summarise(dist_death = mean(dist_death)) %>%
  arrange(as.numeric(respondents)) %>% 
  select(-respondents) %>%
  bind_cols(community) %>%
  select(response_id, dist_death)

##

options(scipen = 999)

p4 <- 
  ggplot(community %>%
           st_drop_geometry() %>%
           drop_na(vote) %>%
           left_join(distances) %>%
           mutate(pct = ntile(dist_death, 100)) %>%
           filter(pct != 1 & pct != 100) %>%
           rename(`dist death (m)` = dist_death) %>%
           drop_na(covid_concern2)) +
  geom_jitter(aes(covid_concern2, `dist death (m)` / 1000, colour = covid_concern2)) +
  stat_summary(aes(covid_concern2, `dist death (m)` / 1000), fun.y = mean, geom = "point", size = 3) +
  scale_color_brewer(palette = 'Set1', name = "concern") +
  xlab("concern with covid") +
  ylab("distance to nearest covid death (km)") +
  coord_flip() + 
  theme_rot() + 
  ggsave("deaths_concern.png", height = 4, width = 8, dpi = 300)

p5 <- 
  ggplot(community %>%
           st_drop_geometry() %>%
           drop_na(vote) %>%
           left_join(distances) %>%
           mutate(pct = ntile(dist_death, 100)) %>%
           filter(pct != 1 & pct != 100) %>%
           rename(`dist death (m)` = dist_death) %>%
           drop_na(covid_concern1)) +
  geom_jitter(aes(covid_concern1, `dist death (m)` / 1000, colour = covid_concern1)) +
  stat_summary(aes(covid_concern1, `dist death (m)` / 1000), fun.y = mean, geom = "point", size = 3) +
  scale_color_brewer(palette = 'Set1', name = "concern") +
  xlab("likelihood of contracting covid") +
  ylab("distance to nearest covid death (km)") +
  coord_flip() + 
  theme_rot() + 
  ggsave("deaths_likelihood.png", height = 4, width = 8, dpi = 300)

unique(pretest$covid_concern2)

