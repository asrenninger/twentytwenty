library(tidyverse)
library(janitor)

##

pretest <- 
  read_csv("data/2020APPCPreTest-2-17-20_March 2, 2020_14.08.csv") %>%
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

ggplot() +
  geom_sf(data = states, aes()) +
  geom_sf(data = pretest %>%
            drop_na(location_longitude, location_latitude) %>%
            st_as_sf(coords = c("location_longitude", "location_latitude"), remove = FALSE, crs = 4326) %>%
            st_transform(102003) %>%
            st_intersection(st_as_sfc(st_bbox(states))) %>%
            mutate(vote = case_when(votechoice == "I am almost certain to vote AGAINST Donald Trump no matter whom the Democrats decide to nominate." ~ "Against Trump",
                                    votechoice == "I am almost certain to vote FOR Donald Trump no matter whom the Democrats nominate for president." ~ "For Trump",
                                    votechoice == "I may vote for or against Donald Trump depending upon whom the Democrats nominate for president." ~ "Undecided",
                                    TRUE ~ votechoice)), 
          aes(colour = vote, fill = vote)) +
  theme_map() +
  ggsave("vote.png", height = 8, width = 11, dpi = 300)


pretest %>%
  drop_na(location_longitude, location_latitude) %>%
  st_as_sf(coords = c("location_longitude", "location_latitude"), remove = FALSE, crs = 4326) %>%
  st_transform(102003) %>%
  st_intersection(st_as_sfc(st_bbox(states))) %>%
  mutate(vote = case_when(votechoice == "I am almost certain to vote AGAINST Donald Trump no matter whom the Democrats decide to nominate." ~ "Against Trump",
                          votechoice == "I am almost certain to vote FOR Donald Trump no matter whom the Democrats nominate for president." ~ "For Trump",
                          votechoice == "I may vote for or against Donald Trump depending upon whom the Democrats nominate for president." ~ "Undecided",
                          TRUE ~ votechoice)) %>%
  mapview::mapview(zcol = "vote")


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

##

tracts <- 
  read_sf("data/tracts.shp") %>%
  mutate(state = str_sub(GEOID10, 1, 2)) %>%
  filter(!str_detect(state, "15|02|60|66|69|72|78")) %>%
  transmute(GEOID = GEOID10, area = ALAND10) %>%
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
  left_join(tracts) %>%
  st_as_sf() %>%
  mutate(pct_latino = latino / (population + 1),
         pct_foreign = foreign / (population + 1),
         pct_black = black / (population + 1),
         pct_white = white / (population + 1),
         density = area / (population + 1),
         churn = moved_nation + moved_abroad / (population + 1),
         unemp_rate = unemployed / labor_force)

##

geolocated <-
  pretest %>%
  drop_na(location_longitude, location_latitude) %>%
  st_as_sf(coords = c("location_longitude", "location_latitude"), remove = FALSE, crs = 4326) %>%
  st_transform(102003) %>%
  st_intersection(st_as_sfc(st_bbox(states))) %>%
  mutate(vote = case_when(votechoice == "I am almost certain to vote AGAINST Donald Trump no matter whom the Democrats decide to nominate." ~ "Against Trump",
                          votechoice == "I am almost certain to vote FOR Donald Trump no matter whom the Democrats nominate for president." ~ "For Trump",
                          votechoice == "I may vote for or against Donald Trump depending upon whom the Democrats nominate for president." ~ "Undecided",
                          TRUE ~ votechoice))

##

community <-
  geolocated %>%
  st_join(census)

##

glimpse(community)

##

library(RColorBrewer)

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
  ggsave("area.png", height = 6, width = 8, dpi = 300)
  
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
         filter(pct != 1 & pct != 100) %>%
         rename(`dist listed companie` = dist_listed_company)) +
  geom_jitter(aes(vote, `dist listed companie`, colour = vote)) +
  stat_summary(aes(vote, `dist listed companie`), fun.y = mean, geom = "point", size = 3) +
  scale_color_brewer(palette = 'Set1') +
  coord_flip() + 
  theme_rot() + 
  ggsave("distance.png", height = 4, width = 6, dpi = 300)




