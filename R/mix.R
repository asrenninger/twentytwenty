library(tidyverse)
library(dtplyr)
library(data.table)

##

library(fs)
library(glue)

##

files <- dir_ls("data/poi")
files <- files[str_detect(files, "core_poi")]

##

poi <- map_df(files, read_csv)

##

poi <- lazy_dt(poi)

phl <- 
  poi %>% filter(region == "PA" & city == "Philadelphia") %>%
  as_tibble()

glimpse(phl)

##

files <- dir_ls("data/safegraph/v1/main-file")

patterns <- 
  reduce(
    map(files[1:5], function(x) {
      read_csv(x) %>%
        filter(safegraph_place_id %in% phl$safegraph_place_id)
    }),
    rbind
  )

##

glimpse(patterns)

##

library(lubridate)

##

march <- filter(patterns, date_range_end < as_date("2020-04-01"))

ordest <- 
  march %>%
  select(safegraph_place_id, visitor_home_cbgs) %>%
  mutate(visitor_home_cbgs = str_remove_all(visitor_home_cbgs, "\\{|\\}"),
         visitor_home_cbgs = str_remove_all(visitor_home_cbgs, "\"|")) %>%
  filter(visitor_home_cbgs != "") %>%
  separate_rows(visitor_home_cbgs, sep = ",") %>%
  separate(visitor_home_cbgs, into = c("cbgs", "visits")) %>%
  mutate(visits = as.numeric(visits))

write_csv(phl, "philly.csv")
write_csv(ordest, "ordest.csv")

##

library(tigris)
library(sf)

metro <- c("Bucks", "Chester", "Delaware", "Montgomery", "Philadelphia", "Burlington", "Camden", "Gloucester", "New Castle")
state <- c("PA", "PA", "PA", "PA", "PA", "NJ", "NJ", "NJ", "DE")

length(metro)
length(state)

##

cbgs <-
  reduce(
    map(1:length(metro), function(x){
      block_groups(state = state[x], county = metro[x], cb = TRUE, class = 'sf')
    }),
    rbind
  )

##

ggplot() +
  geom_sf(data = cbgs, aes()) +
  geom_point(data = phl, aes(x = longitude, y = latitude), size = 0.5) +
  theme_void()

##

st_crs(cbgs)

interactors <- 
  phl %>% 
  st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, crs = 4326) %>%
  st_transform(4269) %>%
  st_join(cbgs) %>%
  st_drop_geometry() %>%
  transmute(safegraph_place_id = safegraph_place_id,
            cbgs_venue = GEOID) %>%
  drop_na(cbgs_venue)

network <- 
  ordest %>%
  left_join(interactors) %>%
  select(cbgs, cbgs_venue, visits) %>%
  mutate(GEOID = str_sub(cbgs_venue, start = 1, end = 5)) %>%
  filter(GEOID == "42101") %>%
  mutate(GEOID = str_sub(cbgs, start = 1, end = 5)) %>%
  filter(GEOID == "42101") 

nodes <- 
  bind_rows(network %>% 
              distinct(cbgs_venue) %>%
              as_tibble() %>%
              rename(cbgs = cbgs_venue),
            network %>% 
              distinct(cbgs) %>%
              as_tibble()) %>%
  distinct()

edges <-
  network %>%
  filter(cbgs != cbgs_venue) %>%
  rename(from = cbgs,
         to = cbgs_venue)

##

library(igraph)
library(tidygraph)

##

graph <- graph_from_data_frame(edges, nodes, directed = FALSE)

graph <-
  graph %>%
  set_edge_attr("weight", value = edges$visits)

plot(graph,
     vertex.size = 0.1,
     vertex.label = '', 
     edge.width = E(graph)$weight / 100,
     alpha = 0.5)

graph <- as_tbl_graph(graph)

geometry <- transmute(cbgs, name = GEOID)

grouped <-
  graph %>%
  activate(nodes) %>%
  left_join(geometry) %>%
  mutate(group = group_louvain()) %>%
  select(name, group, geometry) %>%
  mutate(group = factor(group)) %>%
  as_tibble() %>%
  st_as_sf()

group <-
  grouped %>%
  group_by(group) %>%
  summarise() %>%
  st_centroid() %>%
  pull(group)

labels <-
  grouped %>%
  group_by(group) %>%
  summarise() %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(group = group)

ggplot() +
  geom_sf(data = grouped, aes(fill = group), colour = '#ffffff', size = 0.5, show.legend = FALSE) +
  geom_text(data = labels,
            aes(X, Y, label = group))




