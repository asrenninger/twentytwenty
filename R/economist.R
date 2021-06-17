#### packages

library(tidyverse)
library(janitor)
library(sf)
library(glue)
library(tigris)
library(tidycensus)
library(lubridate)

#### APPC
## read in participants
load("data/07_MergedWeightedDataLong.rdata")
load("data/07_MergedWeightedData.rdata")

#### coordinates
## state, cbg, and lat/lon
coordinates <- 
  haven::read_sav("data/appc/APPC Wave 1_Client_Final 2020-06-19_with weights.sav")  %>%
  transmute(bssrs_match_code = pssrs_match_code,
            GEOID = pcensus_block_group,
            state = pmstate, 
            lat = as.numeric(pabs_latitude),
            lon = as.numeric(pabs_longitude)) %>%
  drop_na()

#### spatial context
## counties and location
counties <- 
  counties(state = "WI", cb = TRUE, resolution = "5m", class = 'sf') %>%
  st_transform(2289)

scene <- c(-87.843278, 42.600639)
point <- 
  st_point(scene) %>% 
  st_sfc(crs = 4326) %>%
  st_transform(2289)

state <- 
  counties %>% 
  st_union() %>% 
  st_combine()

buffers <- rbind(st_buffer(point, 5280 * 5) %>% st_as_sf() %>% mutate(distance = 5, x_min = grab_x(x)), 
                 st_buffer(point, 5280 * 10) %>% st_as_sf() %>% mutate(distance = 10, x_min = grab_x(x)),
                 st_buffer(point, 5280 * 20) %>% st_as_sf() %>% mutate(distance = 20, x_min = grab_x(x)),
                 st_buffer(point, 5280 * 30) %>% st_as_sf() %>% mutate(distance = 30, x_min = grab_x(x)),
                 st_buffer(point, 5280 * 40) %>% st_as_sf() %>% mutate(distance = 40, x_min = grab_x(x)),
                 st_buffer(point, 5280 * 50) %>% st_as_sf() %>% mutate(distance = 50, x_min = grab_x(x))) %>%
  rename(geometry = x)

grab_x <- function(x){
  
  x_min <- x %>% 
    st_coordinates() %>% 
    as_tibble() %>% 
    pull(X) %>% 
    min()
  
  return(x_min)
  
}

   
cropped <- 
  buffers %>% 
  st_intersection(state)

limits <- 
  cropped %>% 
  st_buffer(5280 * 5) %>%
  st_bbox() %>% 
  st_as_sfc() %>%
  st_intersection(state)

coordinates <-
  bind_cols(coordinates %>%
              st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
              st_transform(2289) %>%
              st_intersection(limits) %>% 
              st_coordinates() %>% 
              as_tibble(),
            coordinates %>%
              st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
              st_transform(2289) %>%
              st_intersection(limits)) %>%
  st_as_sf()

#### identify participants
## pick a question
geninfo %>% as_tibble() %>% filter(str_detect(str_to_lower(wordings), "vote"))

# handling
questions <- c("e9_W2", "e9_W4", "e9_W5", "c27_W6")
# protests
questions <- c("i36_W2", "m16_W3", "i36_W4", "m16_W6", "i36_W7", "i36_W8")
# voting
questions <- c("g2a_W1", "g2a_W2", "a4_W3", "g2a_W4", "g40_W5", "g40_W6", "g40_W7")

participants <-
  dfl %>% 
  as_tibble() %>% 
  select(bssrs_match_code, all_of(questions)) %>% 
  mutate(nas = rowSums(is.na(.))) %>%
  filter(nas < 5) %>%
  select(-nas) %>%
  pivot_longer(!bssrs_match_code, names_to = "wave",
               names_transform = list(wave = ~str_remove(.x, ".*_")),
               values_to = "response") %>%
  left_join(coordinates) %>%
  drop_na(lat, lon) %>%
  st_as_sf()

#### calculate distance
## miles from the scene of the crime
distances <- 
  coordinates %>% 
  st_set_crs(2289) %>% 
  mutate(distance = units::drop_units(st_distance(geometry, point, by_element = TRUE) / 5280)) %>%
  select(bssrs_match_code, distance) %>%
  st_drop_geometry()

#### create look
## guides and themes 
guide_continuous <- 
  guide_colorbar(direction = "horizontal",
                 barheight = unit(2, units = "mm"),
                 barwidth = unit(50, units = "mm"),
                 draw.ulim = FALSE,
                 title.position = 'top',
                 label.position = 'bottom',
                 title.hjust = 0.5,
                 label.hjust = 0.5)


guide_discrete <-
  guide_legend(direction = "horizontal",
               keyheight = unit(2, units = "mm"),
               keywidth = unit(20, units = "mm"),
               title.position = 'top',
               label.position = 'bottom',
               title.hjust = 0.5,
               label.hjust = 0.5,
               nrow = 1,
               byrow = TRUE)

theme_map <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          plot.caption = element_text(face = 'bold', colour = 'black'),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(5, 5, 5, 5),
          legend.position = 'bottom'
    )
  
}

#### test plot
## experiment with each layer
ggplot() +
  geom_sf(data = counties %>%
            st_intersection(limits),
          aes(),
          linetype = 1, size = 0.5, fill = '#c7c7c7', colour = '#ffffff') +
  geom_sf(data = cropped, 
          aes(),
          linetype = 2, size = 0.5, fill = NA, colour = '#000000') +
  geom_sf(data = limits,
          aes(),
          linetype = 1, size = 1, fill = NA, colour = '#ffffff') +
  geom_sf(data = counties %>% 
            filter(NAME == "Kenosha"),
          aes(),
          linetype = 1, size = 1.25, fill = NA, colour = '#000000') + 
  geom_label(data = cropped %>%
               filter(distance > 5) %>%
               mutate(content = case_when(distance == 10 ~ paste(distance, "m", sep = " "),
                                          TRUE ~ paste(distance))), 
             aes(x = x_min, y = 226237.7 - 10000, label = content),
             fill = '#c7c7c7', label.size = NA, fontface = 'bold') + 
  geom_point(data = distances %>%
               left_join(coordinates),
             aes(X, Y, colour = distance),
             alpha = 0.5) + 
  scale_colour_gradientn(colours = viridis::cividis(9), 
                         limits = c(0, 60),
                         breaks = c(10, 20, 30, 40, 50),
                         oob = scales::squish,
                         name = "Distance (miles)",
                         guide = guide_continuous) + 
  theme_map() +
  ggsave("economist.png", height = 6, width = 6, dpi = 300)

#### transform data
## long participants + waves + dates + coordinates, wide questions
timing <- 
  longdata %>% 
  as_tibble() %>% 
  select_if(str_detect(names(.), "bssrs_match_code|start_date")) %>% 
  distinct(bssrs_match_code, .keep_all = TRUE) %>% 
  drop_na() %>% 
  pivot_longer(cols = start_date_W1:start_date_W9, names_to = "wave", values_to = "date") %>%
  mutate(wave = str_remove_all(wave, "start_date_"),
         date = str_remove_all(date, " ")) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y")) %>%
  mutate(relative_timing = date > as_date("2020-08-23")) %>%
  filter(bssrs_match_code %in% distances$bssrs_match_code)

timing %>% 
  left_join(participants) %>% 
  drop_na(lat, lon) %>%
  group_by(bssrs_match_code, relative_timing) %>% 
  summarise(protested_flag = sum(response)) %>%
  pivot_wider(id_cols = bssrs_match_code, names_from = relative_timing, values_from = protested_flag) %>%
  rename(before = `FALSE`,
         after = `TRUE`) %>% 
  filter(!is.na(after)) %>%
  mutate(protested_flag = case_when(before > 0 && after > 0 ~ "yes/yes", 
                                    before < 1 && after < 1 ~ "no/no",
                                    before > 0 && after < 1 ~ "yes/no", 
                                    before < 1 && after > 0 ~ "no/yes")) %>%
  group_by(protested_flag) %>%
  summarise(n = n()) %>%
  drop_na(protested_flag)

protests_spatial <- 
  timing %>% 
  left_join(participants) %>% 
  drop_na(lat, lon) %>%
  group_by(bssrs_match_code, relative_timing) %>% 
  summarise(protested_flag = sum(response)) %>%
  pivot_wider(id_cols = bssrs_match_code, names_from = relative_timing, values_from = protested_flag) %>%
  rename(before = `FALSE`,
         after = `TRUE`) %>% 
  filter(!is.na(after)) %>%
  mutate(protested_flag = case_when(before > 0 && after > 0 ~ "yes/yes", 
                                    before < 1 && after < 1 ~ "no/no",
                                    before > 0 && after < 1 ~ "yes/no", 
                                    before < 1 && after > 0 ~ "no/yes")) %>% 
  left_join(coordinates)

plot_protests <- 
  ggplot() +
  geom_sf(data = counties %>%
            st_intersection(limits),
          aes(),
          linetype = 1, size = 0.5, fill = '#c7c7c7', colour = '#ffffff') +
  geom_sf(data = cropped, 
          aes(),
          linetype = 2, size = 0.5, fill = NA, colour = '#000000') +
  geom_sf(data = limits,
          aes(),
          linetype = 1, size = 1, fill = NA, colour = '#ffffff') +
  geom_sf(data = counties %>% 
            filter(NAME == "Kenosha"),
          aes(),
          linetype = 1, size = 1.25, fill = NA, colour = '#000000') + 
  geom_label(data = cropped %>%
               filter(distance > 5) %>%
               mutate(content = case_when(distance == 10 ~ paste(distance, "m", sep = " "),
                                          TRUE ~ paste(distance))), 
             aes(x = x_min, y = 226237.7 - 10000, label = content),
             fill = '#c7c7c7', label.size = NA, fontface = 'bold') + 
  geom_point(data = protests_spatial %>% 
               mutate(joined = if_else(protested_flag == "no/yes", "joined", "abstained")) %>% 
               arrange(joined),
             aes(X, Y, colour = joined, alpha = joined)) + 
  scale_alpha_discrete(range = c(0.1, 1), guide = 'none') + 
  scale_colour_manual(values = viridisLite::mako(9)[c(2, 8)], 
                      name = "Protested (change after Blake death)",
                      na.translate = FALSE,
                      guide = guide_discrete) + 
  labs(title = "Protesting",
       subtitle = "Have you or an acquaintance been to a protest?") + 
  theme_map() +
  ggsave("economist_protests.png", height = 6, width = 6, dpi = 300)

handling_spatial <- 
  timing %>% 
  left_join(participants) %>% 
  drop_na(lat, lon) %>%
  mutate(response = case_when(str_detect(as.character(response), "difference") ~ "The same",
                              TRUE ~ as.character(response))) %>% 
  group_by(bssrs_match_code, relative_timing) %>%
  summarise(handling_flag = paste(response, collapse = "/")) %>%
  pivot_wider(id_cols = bssrs_match_code, names_from = relative_timing, values_from = handling_flag) %>%
  rename(before = `FALSE`,
         after = `TRUE`) %>%
  mutate(handling = case_when(str_detect(before, "Trump") & str_detect(after, "Trump") ~ "held trump",
                              str_detect(before, "Trump") & str_detect(after, "Biden") ~ "toward biden",
                              str_detect(before, "Biden") & str_detect(after, "Trump") ~ "toward trump",
                              str_detect(before, "same") & str_detect(after, "Biden") ~ "toward biden",
                              str_detect(before, "same") & str_detect(after, "Trump") ~ "toward trump",
                              str_detect(before, "Biden") & str_detect(after, "Biden") ~ "held biden",
                              str_detect(before, "same") & str_detect(after, "same") ~ "indifferent")) %>%
   left_join(coordinates)

plot_handling <- 
  ggplot() +
  geom_sf(data = counties %>%
            st_intersection(limits),
          aes(),
          linetype = 1, size = 0.5, fill = '#c7c7c7', colour = '#ffffff') +
  geom_sf(data = cropped, 
          aes(),
          linetype = 2, size = 0.5, fill = NA, colour = '#000000') +
  geom_sf(data = limits,
          aes(),
          linetype = 1, size = 1, fill = NA, colour = '#ffffff') +
  geom_sf(data = counties %>% 
            filter(NAME == "Kenosha"),
          aes(),
          linetype = 1, size = 1.25, fill = NA, colour = '#000000') + 
  geom_label(data = cropped %>%
               filter(distance > 5) %>%
               mutate(content = case_when(distance == 10 ~ paste(distance, "m", sep = " "),
                                          TRUE ~ paste(distance))), 
             aes(x = x_min, y = 226237.7 - 10000, label = content),
             fill = '#c7c7c7', label.size = NA, fontface = 'bold') + 
  geom_point(data = handling_spatial %>% 
               mutate(joined = if_else(handling == "toward trump", "toward trump", "no change")) %>%
               arrange(joined),
             aes(X, Y, colour = joined, alpha = joined)) + 
  scale_alpha_discrete(range = c(0.1, 1), guide = 'none') + 
  scale_colour_manual(values = viridisLite::mako(9)[c(2, 8)], 
                      name = "Slides Trumpward (before/after Blake death)",
                      na.translate = FALSE,
                      guide = guide_discrete) + 
  labs(title = "Handling race",
       subtitle = "Who would do a better job on Race?") + 
  theme_map() +
  ggsave("economist_handling.png", height = 6, width = 6, dpi = 300)

coordinates_full <- 
  haven::read_sav("data/appc/APPC Wave 1_Client_Final 2020-06-19_with weights.sav")  %>%
  transmute(bssrs_match_code = pssrs_match_code,
            GEOID = pcensus_block_group,
            state = pmstate, 
            lat = as.numeric(pabs_latitude),
            lon = as.numeric(pabs_longitude)) %>%
  drop_na()

coordinates_full <-
  bind_cols(coordinates_full %>%
              st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
              st_transform(2289) %>%
              st_intersection(state) %>% 
              st_coordinates() %>% 
              as_tibble(),
            coordinates_full %>%
              st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
              st_transform(2289) %>%
              st_intersection(state)) %>%
  st_as_sf()

distances_full <- 
  coordinates_full %>% 
  st_set_crs(2289) %>% 
  mutate(distance = units::drop_units(st_distance(geometry, point, by_element = TRUE) / 5280)) %>%
  select(bssrs_match_code, distance) %>%
  st_drop_geometry()

distances_full %>% write_csv("kenosha_distances.csv")

participants_full <-
  dfl %>% 
  as_tibble() %>% 
  select(bssrs_match_code, all_of(questions)) %>% 
  mutate(nas = rowSums(is.na(.))) %>%
  filter(nas < 5) %>%
  select(-nas) %>%
  pivot_longer(!bssrs_match_code, names_to = "wave",
               names_transform = list(wave = ~str_remove(.x, ".*_")),
               values_to = "response") %>%
  left_join(coordinates_full) %>%
  drop_na(lat, lon) %>%
  st_as_sf()

timing_full <- 
  longdata %>% 
  as_tibble() %>% 
  select_if(str_detect(names(.), "bssrs_match_code|start_date")) %>% 
  distinct(bssrs_match_code, .keep_all = TRUE) %>% 
  drop_na() %>% 
  pivot_longer(cols = start_date_W1:start_date_W9, names_to = "wave", values_to = "date") %>%
  mutate(wave = str_remove_all(wave, "start_date_"),
         date = str_remove_all(date, " ")) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y")) %>%
  mutate(relative_timing = date > as_date("2020-08-23")) %>%
  filter(bssrs_match_code %in% distances_full$bssrs_match_code)

handling_spatial_full <- 
  timing_full %>% 
  left_join(participants_full) %>% 
  drop_na(lat, lon) %>%
  mutate(response = case_when(str_detect(as.character(response), "difference") ~ "The same",
                              TRUE ~ as.character(response))) %>% 
  group_by(bssrs_match_code, relative_timing) %>%
  summarise(handling_flag = paste(response, collapse = "/")) %>%
  pivot_wider(id_cols = bssrs_match_code, names_from = relative_timing, values_from = handling_flag) %>%
  rename(before = `FALSE`,
         after = `TRUE`) %>%
  mutate(handling = case_when(str_detect(before, "Trump") & str_detect(after, "Trump") ~ "held trump",
                              str_detect(before, "Trump") & str_detect(after, "Biden") ~ "toward biden",
                              str_detect(before, "Biden") & str_detect(after, "Trump") ~ "toward trump",
                              str_detect(before, "same") & str_detect(after, "Biden") ~ "toward biden",
                              str_detect(before, "same") & str_detect(after, "Trump") ~ "toward trump",
                              str_detect(before, "Biden") & str_detect(after, "Biden") ~ "held biden",
                              str_detect(before, "same") & str_detect(after, "same") ~ "indifferent")) %>%
  left_join(coordinates_full)

handling_spatial %>% 
  mutate(joined = if_else(handling == "toward trump", 1, 0)) %>%
  left_join(distances_full) %>% 
  select(-geometry) %>% 
  glm(data = ., 
      joined ~ distance, family = binomial(link="logit")) %>%
  summary()

library(patchwork)

plot <- plot_handling + plot_protests
ggsave(plot, filename = "economist_combined.png", height = 8, width = 12, dpi = 300)

#### election results
## use precincts instead of participants
vote <- read_sf("data/times/precinct-results.gpkg")

vote_trimmed <- 
  vote %>% 
  filter(str_sub(GEOID, 1, 2) == "55") %>% 
  st_transform(2289) %>%
  st_intersection(limits)

redblu <- read_csv("https://raw.githubusercontent.com/asrenninger/palettes/master/redblu.txt", col_names = FALSE) %>% pull(X1)

plot_vote <- 
  ggplot() +
  geom_sf(data = vote_trimmed,
          aes(fill = pct_dem_lead), 
          colour = NA, size = 0) + 
  geom_sf(data = counties %>%
            st_intersection(limits),
          aes(),
          linetype = 1, size = 0.5, fill = NA, colour = '#ffffff') +
  geom_sf(data = cropped, 
          aes(),
          linetype = 2, size = 0.5, fill = NA, colour = '#000000') +
  geom_sf(data = limits,
          aes(),
          linetype = 1, size = 1, fill = NA, colour = '#ffffff') +
  geom_sf(data = counties %>% 
            filter(NAME == "Kenosha"),
          aes(),
          linetype = 1, size = 1.25, fill = NA, colour = '#000000') + 
  geom_label(data = cropped %>%
               filter(distance > 5) %>%
               mutate(content = case_when(distance == 10 ~ paste(distance, "m", sep = " "),
                                          TRUE ~ paste(distance))), 
             aes(x = x_min, y = 226237.7 - 10000, label = content),
             fill = '#ffffff', label.size = NA, fontface = 'bold', alpha = 0.5) + 
  scale_alpha_discrete(range = c(0.1, 1), guide = 'none') + 
  scale_fill_gradient2(low = redblu[1],
                       high = redblu[9], 
                       midpoint = 0, 
                       name = "Democratic margin",
                       guide = guide_continuous) + 
  labs(title = "Votes in 2020",
       subtitle = "Precinct results around Kenosha") + 
  theme_map() +
  ggsave("economist_votes.png", height = 6, width = 6, dpi = 300)

vote_2016 <- 
  read_sf("data/times/wi_2016/wi_2016.shp") %>% 
  glimpse() %>%
  transmute(GEOID = str_sub(GEOID, 1, 5),
            trump = G16PRERTRU,
            clinton = G16PREDCLI,
            total = G16PRERTRU + G16PREDCLI + G16PRECCAS + G16PRELJOH + G16PREGSTE + G16PREOTH + 1) %>% 
  transmute(GEOID, 
            margin_2016 = (clinton - trump) / total, 
            total_2016 = total - 1) %>%
  filter(total_2016 > 0) %>%
  st_transform(2289)
  
vote_2020 <- transmute(vote_trimmed, 
                       precinct = GEOID,
                       margin_2020 = (votes_dem - votes_rep) /  votes_total)

shape <- transmute(vote_2020, precinct)

part_1 <- 
  vote_2020 %>% 
  st_join(st_centroid(vote_2016)) %>%
  st_drop_geometry() %>% 
  group_by(GEOID, precinct, margin_2020) %>% 
  summarise(margin_2016 = weighted.mean(margin_2016, total_2016)) %>% 
  ungroup() %>%
  left_join(shape) %>% 
  drop_na(margin_2016) %>%
  st_as_sf()

part_2 <-
  vote_2020 %>% 
  filter(!precinct %in% part_1$precinct) %>%  
  st_join(vote_2016) %>%
  st_drop_geometry() %>% 
  group_by(GEOID, precinct, margin_2020) %>% 
  summarise(margin_2016 = weighted.mean(margin_2016, total_2016)) %>% 
  ungroup() %>%
  left_join(shape) %>% 
  st_as_sf()

whole <- 
  bind_rows(part_1, part_2) %>% 
  st_as_sf() %>% 
  mutate(change = margin_2016 - margin_2020) %>% 
  select(GEOID, precinct, margin_2020, margin_2016, change) %>% 
  rename(geometry = geom)

plot_vote <- 
  ggplot() +
  geom_sf(data = whole,
          aes(fill = change * 100), 
          colour = NA, size = 0) + 
  geom_sf(data = counties %>%
            st_intersection(limits),
          aes(),
          linetype = 1, size = 0.5, fill = NA, colour = '#ffffff') +
  geom_sf(data = cropped, 
          aes(),
          linetype = 2, size = 0.5, fill = NA, colour = '#000000') +
  geom_sf(data = limits,
          aes(),
          linetype = 1, size = 1, fill = NA, colour = '#ffffff') +
  geom_sf(data = counties %>% 
            filter(NAME == "Kenosha"),
          aes(),
          linetype = 1, size = 1.25, fill = NA, colour = '#000000') + 
  geom_label(data = cropped %>%
               filter(distance > 5) %>%
               mutate(content = case_when(distance == 10 ~ paste(distance, "m", sep = " "),
                                          TRUE ~ paste(distance))), 
             aes(x = x_min, y = 226237.7 - 10000, label = content),
             fill = '#ffffff', label.size = NA, fontface = 'bold', alpha = 0.5) + 
  scale_alpha_discrete(range = c(0.1, 1), guide = 'none') + 
  scale_fill_gradient2(low = redblu[9],
                       high = redblu[1], 
                       midpoint = 0, 
                       limits = c(-50, 50),
                       breaks = c(-50, -25, 0, 25, 50),
                       labels = c("Toward Biden 50", "25", "0", "25", "50 Toward Trump"),
                       oob = scales::squish,
                       name = "Margin change, 2016-2020 (percentage points)",
                       guide =   guide_colorbar(direction = "horizontal",
                                                barheight = unit(2, units = "mm"),
                                                barwidth = unit(120, units = "mm"),
                                                draw.ulim = FALSE,
                                                title.position = 'top',
                                                label.position = 'bottom',
                                                title.hjust = 0.5,
                                                label.hjust = 0.5)) + 
  labs(title = "Votes in 2020",
       subtitle = "Precinct results around Kenosha") + 
  theme_map() +
  ggsave("economist_votes.png", height = 10, width = 10, dpi = 300)

theme_hor <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(size = 0.5, colour = 'black'),
          axis.line.y = element_blank(),
          axis.ticks.x = element_line(size = 0.5, colour = 'black'),
          axis.ticks.y = element_line(size = 0.1, colour = 'grey50'),
          axis.text.x = element_text(face = 'bold'),
          axis.text.y = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(20, 20, 20, 20)
    )
}


ward <- 
  whole %>% 
  st_drop_geometry() %>% 
  drop_na() %>% 
  filter(change == max(change)) %>% 
  pull(precinct) %>% 
  str_remove(".*-") %>% 
  str_remove("0") %>% 
  str_to_title()

plot_distribution <- 
  whole %>% 
  st_drop_geometry() %>% 
  ggplot(aes(change)) +
  geom_density(fill = redblu[1], colour = redblu[1], alpha = 0.75) +
  geom_vline(xintercept = max(na.omit(whole$change)),
             linetype = 2, size = 1, colour = '#c7c7c7') +
  annotate(geom = 'text', x = 0.9, y = 3, label = ward, angle = 90, fontface = 'bold') + 
  labs(title = "Distribution of Changes",
       subtitle = "Kenosha City and the Largest Shift",
       x = "", y = "") + 
  theme_hor() + 
  ggsave("economist_distribution.png", height = 6, width = 8, dpi = 300)

library(patchwork)

plot <- plot_vote + plot_distribution
ggsave(plot, filename = "economist_combined_new.png", height = 11, width = 17, dpi = 300)

regression <- 
  whole %>% 
  st_centroid() %>%
  st_set_crs(2289) %>% 
  mutate(distance = units::drop_units(st_distance(geometry, point, by_element = TRUE) / 5280)) %>%
  st_drop_geometry() %>%
  select(precinct, distance) %>%
  left_join(whole) %>%
  st_as_sf()

spatial <- 
  regression %>% 
  drop_na() %>%
  st_as_sf() %>% 
  as('Spatial')

library(spgwr)

bandwidth <- gwr.sel(change ~ distance,
                     data = spatial)

geogress <- gwr(change ~ distance,
                data = spatial,
                bandwidth = bandwidth)

plot(st_as_sf(geogress$SDF))
st_as_sf(geogress$SDF)

highlight <- 
  regression %>% 
  drop_na() %>% 
  filter(change == max(change)) %>% 
  select(change, distance)

plot_scatter <- 
  ggplot(regression, 
       aes(distance, change * 100)) +
  geom_vline(xintercept = highlight$distance,
             linetype = 2, size = 0.5, colour = '#c7c7c7') +
  geom_hline(yintercept = highlight$change * 100,
             linetype = 2, size = 0.5, colour = '#c7c7c7') +
  geom_point(colour = redblu[1], alpha = 0.75) +
  annotate(geom = 'text', x = highlight$distance + 10, y = (highlight$change * 100) + 10, hjust = 0.5, label = ward, angle = 0, fontface = 'bold') + 
  labs(title = "Relationship of Changes to Distance",
       subtitle = "Kenosha City and the Largest Shift",
       x = "distance (m)", y = "change in margin (percent)") + 
  theme_ver() + 
  ggsave("economist_scatter.png", height = 6, width = 8, dpi = 300)

plot <- plot_scatter + plot_distribution
ggsave(plot, filename = "economist_combined_new.png", height = 10, width = 20, dpi = 300)
