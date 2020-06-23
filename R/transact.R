library(tidyverse)
library(vroom)
library(data.table)
library(dtplyr)

spend <- 
  vroom("data/transactions/cut-1-daily-spend-by-zip/2020-06-09/cut-1-daily-spend-by-zip-20200417-20200525.csv.gz",
           col_names = FALSE) %>%
  set_names("date", "zcta", "num_cards", "num_trans", "spend")

glimpse(spend)

##

library(tigris)
library(sf)

##

options(tigris_use_cache = TRUE)

##

states <- c("FL", "WI", "MI", "PA", "OH")

zctas <- 
  reduce(
    map(states, 
        function(x){
          zctas(cb = TRUE, state = x, class = 'sf') %>%
            mutate(state = x)
        }),
    rbind
  )

##

library(lubridate)

##

counties <- counties(cb = TRUE, class = 'sf')

state <- "42|26|55|12|39" 
names <- "Luzerne|Macomb|Racine|Kenosha|Pinellas"

bonus <- filter(counties, NAME == "Montgomery" & STATEFP == "39")
study <- filter(counties, str_detect(NAME, names)) 

complete <- rbind(study, bonus)

filtered_zips <- 
  st_intersection(zctas, complete) %>%
  transmute(GEOID = GEOID,
            zcta = ZCTA5CE10,
            county = NAME,
            state = state)

##

filtered_spend <-
  spend %>%
  filter(date > as_date('2018-12-31')) %>%
  filter(zcta %in% filtered_zips$zcta)

crosswalk <- st_drop_geometry(filtered_zips)

rm(spend)

timeseries <- 
  filtered_spend %>%
  left_join(crosswalk) %>%
  group_by(county, date) %>%
  summarise(spend = sum(spend),
            num_trans = sum(num_trans),
            num_cards = sum(num_cards)) %>%
  mutate(spend_rate = spend / num_cards,
         spend_size = spend / num_trans) %>%
  mutate(spend_lag1 = lag(spend_rate)) %>%
  mutate(spend_lag2 = lag(spend_lag1), 
         spend_lag3 = lag(spend_lag2),
         spend_lag4 = lag(spend_lag3),
         spend_lag5 = lag(spend_lag4)) %>%
  mutate(rolling_avg = (spend_lag1 + spend_lag2 + spend_lag3 + spend_lag4 + spend_lag5) / 5) %>%
  select(-spend_lag1, -spend_lag2, -spend_lag3, -spend_lag4,  -spend_lag5) %>%
  mutate(day = yday(date),
         year = year(date))

library(RColorBrewer)

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

ggplot(timeseries, 
       aes(date, rolling_avg, colour = factor(year))) +
  geom_line(size = 0.74, show.legend = FALSE) +
  scale_color_brewer(palette = 'Set1', name = "year") + 
  xlab("day of year") +
  ylab("spending per person (rolling average)") +
  facet_wrap(~ county) +
  theme_hor() +
  ggsave("transactions.png", height = 6, width = 10, dpi = 300)

