options(stringsAsFactors = FALSE)

new

new <- str_split(test, "\n\n")
new <- as.data.frame(new)
new <- new[-nrow(new), ]
new <- as.data.frame(new)

##

col <- c()

for (i in 1:(nrow(new) / 4)){
  row <- c(i, i, i, i)
  col <- c(col, row)
}

col

new <- cbind(new, col)

##

library(tidyverse)

##

tib <- 
  new %>%
  as_tibble() %>%
  rename(info = new, 
         group = col) %>%
  mutate(labels = rep(c("facility", "location", "cases", "deaths"), times = n()/4)) %>%
  select(group, labels, info) %>%
  pivot_wider(names_from = labels, values_from = info) %>%
  mutate(cases = as.numeric(cases),
         deaths = as.numeric(deaths))

##

library(ggmap)

##

register_google(key = "YOURKEY")

##

data_geocoded <- 
  tib %>%
  mutate(address = glue("{facility}, {location}")) %>%
  mutate_geocode(location = location, source = "google", output = "latlon")

##

shape <- st_as_sf(data_geocoded, coords = c("lon", "lat"), crs = 4326)

plot(shape)

##

state <- "42|26|55|12|39" 
names <- "Luzerne|Macomb|Racine|Kenosha|Pinellas"

care_counties <- 
  shape %>%
  st_transform(4269) %>%
  st_join(counties) %>%
  st_drop_geometry() %>%
  filter(str_detect(STATEFP, state) & str_detect(NAME, names))

care_regions <- 
  shape %>%
  st_transform(4269) %>%
  st_buffer(0.5) %>%
  st_join(counties) %>%
  st_drop_geometry() %>%
  filter(str_detect(STATEFP, state) & str_detect(NAME, names))

care_counties$NAME

care_regions %>%
  mutate(study_area = case_when(NAME == "Racine" ~ "Racine/Kenosha",
                                NAME == "Kenosha" ~ "Racine/Kenosha",
                                TRUE ~ NAME)) %>%
  group_by(study_area) %>%
  summarise(cases = sum(cases),
            deaths = sum(deaths))




