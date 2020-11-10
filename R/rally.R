library(rvest)
library(glue)
library(ggmap)

register_google(key = "AIzaSyATZNqcW_htUb8P6ZgBV9nOMcnvkk5jNI0")

rallies <-
  html("https://en.wikipedia.org/wiki/List_of_post-election_Donald_Trump_rallies") %>%
  html_nodes('table') %>%
  magrittr::extract2(8) %>%
  html_table() %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(address = glue("{venue}, {city}, {state}"),
         date_of_rally = str_remove_all(date_of_rally, "\\[.*?\\]"),
         date_of_rally = str_remove_all(date_of_rally, ",")) %>%
  separate(col = date_of_rally, into = c("weekday", "month", "day", "year"), sep = " ") %>%
  mutate(date = glue("{month} {day}, {year}")) %>%
  mutate(date = mdy(date)) %>%
  mutate_geocode(location = address, source = "google", output = "latlon")

rallies <- 
  rallies %>% 
  st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326) %>%
  st_transform(2163)

rallies %>% select(date) %>% plot()

library(tigris)

counties <- 
  counties(class = "sf", cb = TRUE, resolution = '5m') %>%
  filter(!str_detect(STATEFP, "15|02|60|66|69|72|78")) %>%
  select(GEOID) %>%
  st_transform(2163) 

##

states <- 
  states(class = "sf", cb = TRUE, resolution = '5m') %>%
  filter(!str_detect(STATEFP, "15|02|60|66|69|72|78")) %>%
  select(NAME, GEOID) %>%
  st_transform(2163) 

##

coords <- 
  rallies %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(rallies) %>%
  select(-geometry)

##

library(gganimate)

##

anim <- 
  ggplot() +
  geom_sf(data = states,
          aes(), colour = 'white', lwd = 0.5, alpha = 0.25) +
  geom_path(data = coords %>%
              filter(date > as_date('2020-09-01')), 
            aes(x = X, y = Y), colour = 'maroon') +
  transition_manual(date, cumulative = TRUE) +
  labs(title = "TRUMP CAMPAIGN RALLIES", subtitle = "{lubridate::wday(current_frame, label = TRUE, abbr = FALSE)}, {lubridate::month(current_frame, label = TRUE, abbr = FALSE)} {lubridate::day(current_frame)}") +
  theme_map()

anim_save("rallies.gif", animation = anim, 
          height = 600, width = 800, fps = 2,
          start_pause = 0, end_pause = 2)
  
