library(foreign)
library(tidyverse)
library(janitor)

wave <- read.spss("data/appc/WAVE 1 SURVEY-selected/APPC Wave 1_Client_Prelim 2020-05-14 0600.sav")
  
coords <- 
  wave %>%
  as_tibble() %>%
  clean_names() %>%
  transmute(lat = as.numeric(pabs_longitude), 
            lng = as.numeric(pabs_latitude),
            userid = userid,
            pssrs_match_code = pssrs_match_code,
            cbg = pcensus_block_group)

##

glimpse(coords)

##

library(tidycensus)

##

area <- 
  get_acs(geography = "county", 
          variables = c("B19083_001", "B06011_001", "B25077_001", "B25113_001"), 
          year = 2018, output = "wide") %>% 
  rename(gini_coeff = B19083_001E,
         med_income = B06011_001E,
         home_price = B25077_001E,
         rent_price = B25113_001E) %>%
  select(GEOID, gini_coeff, med_income, home_price, rent_price)

##

race <- 
  get_acs(geography = "county", 
          variables = c("B02001_002", "B02001_003"), 
          year = 2018, output = "wide") %>% 
  rename(white_num = B02001_002E,
         black_num = B02001_003E) %>%
  select(GEOID, white_num, black_num)

##

work <-
  get_acs(geography = "county", variables = c("C18120_002", "C18120_006", "C18120_003"),
          year = 2018, output = "wide") %>%
  rename(lab_force = C18120_002E,
         tot_un = C18120_006E,
         tot_em = C18120_003E) %>%
  select(GEOID, lab_force, tot_un, tot_em)

##

live <- 
  get_acs(geography = "county", variables = c("B01001_001", "B11016_001", "B01002_001E", "B06009_005E"),
          year = 2018, output = "wide") %>%
  rename(population = B01001_001E,
         households = B11016_001E,
         med_age = B01002_001E,
         tot_bachelors = B06009_005E) %>%
  select(GEOID, population, households, med_age, tot_bachelors)

##

fors <- 
  get_acs(geography = "county", variables = c("B05006_124", "B05006_001"),
          year = 2018, output = "wide") %>%
  rename(tot_latinos = B05006_124E,
         tot_foreign = B05006_001E) %>%
  select(GEOID, tot_latinos, tot_foreign)

##

outs <- 
  get_acs(geography = "county", variables = c("B07001_033", "B07001_081"),
          year = 2016, output = "wide") %>%
  rename(tot_moved_nation = B07001_033E,
         tot_moved_abroad = B07001_081E) %>%
  select(GEOID, tot_moved_nation, tot_moved_abroad)

##

move <- get_estimates(geography = "county",
                      variables = "RNETMIG", 
                      year = 2018) %>%
  rename(migration = value) %>%
  select(-variable)

##

library(lubridate)

##

covid <- 
  read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-04-15')) %>%
  rename(GEOID = fips) %>%
  group_by(GEOID) %>%
  filter(date == min(date) | date == max(date)) %>%
  mutate(period = case_when(date == min(date) ~ "apr",
                            date == max(date) ~ "may")) %>%
  ungroup() %>%
  arrange(desc(GEOID)) %>%
  select(GEOID, period, cases, deaths)

counts <- 
  covid %>%
  transmute(GEOID = GEOID, 
            deaths = deaths,
            cases = cases,
            period = period) %>%
  pivot_wider(names_from = period, values_from = c(cases, deaths), 
              values_fill = list(cases = 0, deaths = 0),
              values_fn = list(deaths = max, cases = max))

##

county_level <-
  move %>%
  left_join(area) %>%
  left_join(live) %>%
  left_join(counts) %>%
  rename_if(is.numeric, paste0, "_county")

##

library(tigris)
library(sf)

##

counties <- 
  counties(class = 'sf', cb = TRUE) %>%
  transmute(GEOID = GEOID, 
            STATEFP,
            COUNTYFP,
            area = ALAND) %>%
  filter(!str_detect(STATEFP, "15|02|60|66|69|72|78"))

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

tracts <- read_sf("data/tracts.shp")

##

area <- map_df(us, function(x) {
  get_acs(geography = "tract", state = x,
          variables = c("B19083_001", "B06011_001", "B25077_001", "B25113_001"), 
          year = 2018, output = "wide") %>% 
    rename(gini_coeff = B19083_001E,
           med_income = B06011_001E,
           home_price = B25077_001E,
           rent_price = B25113_001E) %>%
    select(GEOID, gini_coeff, med_income, home_price, rent_price)
})

race <- map_df(us, function(x) {
  get_acs(geography = "tract", state = x,
          variables = c("B02001_002", "B02001_003"), 
          year = 2018, output = "wide") %>% 
    rename(white_num = B02001_002E,
           black_num = B02001_003E) %>%
    select(GEOID, white_num, black_num)
})

work <- map_df(us, function(x) {
  get_acs(geography = "tract", state = x, 
          variables = c("C18120_002", "C18120_006", "C18120_003"),
          year = 2018, output = "wide") %>%
    rename(lab_force = C18120_002E,
           tot_un = C18120_006E,
           tot_em = C18120_003E) %>%
    select(GEOID, lab_force, tot_un, tot_em) 
})

live <- map_df(us, function(x) {
  get_acs(geography = "tract", state = x,
          variables = c("B01001_001", "B11016_001", "B01002_001E", "B06009_005E"),
          year = 2018, output = "wide") %>%
    rename(population = B01001_001E,
           households = B11016_001E,
           med_age = B01002_001E,
           tot_bachelors = B06009_005E) %>%
    select(GEOID, population, households, med_age, tot_bachelors) 
})

fors <- map_df(us, function(x) {
  get_acs(geography = "tract", state = x,
          variables = c("B05006_124", "B05006_001"),
          year = 2018, output = "wide") %>%
    rename(tot_latinos = B05006_124E,
           tot_foreign = B05006_001E) %>%
    select(GEOID, tot_latinos, tot_foreign) 
})

outs <- map_df(us, function(x) {
  get_acs(geography = "tract", state = x,
          variables = c("B07001_033", "B07001_081"),
          year = 2018, output = "wide") %>%
    rename(tot_moved_nation = B07001_033E,
           tot_moved_abroad = B07001_081E) %>%
    select(GEOID, tot_moved_nation, tot_moved_abroad) 
})

##

tract_level <-
  area %>%
  left_join(live) %>%
  left_join(work) %>%
  left_join(race) %>%
  left_join(fors) %>%
  left_join(outs)

##

geo <- 
  coords %>%
  mutate(GEOID = str_sub(cbg, 1, 5)) %>%
  left_join(county_level) %>%
  mutate(GEOID = str_sub(cbg, 1, 11)) %>%
  left_join(tract_level)

write_csv(geo, "wave_one_geo.csv")


