library(tidyverse)
library(lubridate)
library(glue)

geographies <- 
  read_csv("data/chetty/data/GeoIDs - County.csv") %>% 
  transmute(GEOID = str_pad(countyfips, width = 5, side = 'left',  pad = "0"),
            state = statename,
            county = countyname)

data <- 
  read_csv("data/chetty/data/Affinity - County - Daily.csv") %>%
  transmute(date = as_date(glue("{year}-{month}-{day}")),
            GEOID = str_pad(countyfips, width = 5, side = 'left', pad = "0"),
            spend = as.numeric(spend_all)) %>% 
  drop_na() %>%
  left_join(geographies)

ggplot(data) +
  geom_line(aes(date, spend, group = GEOID), colour = '#E5E5E3') +
  stat_summary(aes(date, spend), fun = mean, geom = 'line', size = 1, linetype = 2) +
  geom_line(data = data %>%
              filter(str_detect(state, "Michigan|Pennsylvania|Wisconsin|Florida"),
                     str_detect(county, "Racine|Kenosha|Macomb|Pinellas|Luzerne")),
            aes(date, spend, group = GEOID, colour = county), size = 1) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_colour_brewer(palette = 'Set1') +
  labs(title = "Spend", x = "", y = "") +
  theme_hor() +
  ggsave("spendxcounty.png", height = 6, width = 8, dpi = 300)

data <- 
  read_csv("data/chetty/data/Google Mobility - County - Daily.csv") %>%
  transmute(date = as_date(glue("{year}-{month}-{day}")),
            GEOID = str_pad(countyfips, width = 5, side = 'left', pad = "0"),
            mobility = as.numeric(gps_away_from_home)) %>% 
  drop_na() %>%
  left_join(geographies)

ggplot(data) +
  geom_line(aes(date, mobility, group = GEOID), colour = '#E5E5E3') +
  stat_summary(aes(date, mobility), fun = mean, geom = 'line', size = 1, linetype = 2) +
  geom_line(data = data %>%
              filter(str_detect(state, "Michigan|Pennsylvania|Wisconsin|Florida"),
                     str_detect(county, "Racine|Kenosha|Macomb|Pinellas|Luzerne")),
            aes(date, mobility, group = GEOID, colour = county), size = 1) +
  scale_y_continuous(limits = c(-0.4, 0.1)) +
  scale_colour_brewer(palette = 'Set1') +
  labs(title = "Mobility", x = "", y = "") +
  theme_hor() +
  ggsave("mobilityxcounty.png", height = 6, width = 8, dpi = 300)

data <- 
  read_csv("data/chetty/data/Google Mobility - County - Daily.csv") %>%
  transmute(date = as_date(glue("{year}-{month}-{day}")),
            GEOID = str_pad(countyfips, width = 5, side = 'left', pad = "0"),
            mobility = as.numeric(gps_away_from_home)) %>% 
  drop_na() %>%
  left_join(geographies)

ggplot(data) +
  geom_line(aes(date, mobility, group = GEOID), colour = '#E5E5E3') +
  stat_summary(aes(date, mobility), fun = mean, geom = 'line', size = 1, linetype = 2) +
  geom_line(data = data %>%
              filter(str_detect(state, "Michigan|Pennsylvania|Wisconsin|Florida"),
                     str_detect(county, "Racine|Kenosha|Macomb|Pinellas|Luzerne")),
            aes(date, mobility, group = GEOID, colour = county), size = 1) +
  scale_y_continuous(limits = c(-0.4, 0.05)) +
  scale_colour_brewer(palette = 'Set1') +
  labs(title = "Mobility", x = "", y = "") +
  theme_hor() +
  ggsave("mobilityxcounty.png", height = 6, width = 8, dpi = 300)

states <- 
  read_csv("data/chetty/data/GeoIDs - State.csv") %>%
  transmute(GEOID = str_pad(statefips, width = 2, side = 'left',  pad = "0"),
            state = statename)

data <- 
  read_csv("data/chetty/data/Employment Combined - State - Daily.csv")  %>%
  transmute(date = as_date(glue("{year}-{month}-{day}")),
            GEOID = str_pad(statefips, width = 2, side = 'left', pad = "0"),
            employment = as.numeric(emp_combined),
            low = as.numeric(emp_combined_inclow),
            middle = as.numeric(emp_combined_incmiddle),
            high = as.numeric(emp_combined_inchigh)) %>% 
  drop_na() %>%
  left_join(states)

ggplot(data) +
  geom_line(aes(date, employment, group = GEOID), colour = '#E5E5E3') +
  stat_summary(aes(date, employment), fun = mean, geom = 'line', size = 1, linetype = 2) +
  geom_line(data = data %>%
              filter(str_detect(state, "Michigan|Pennsylvania|Wisconsin|Florida")),
            aes(date, employment, group = GEOID, colour = state), size = 1) +
  scale_colour_brewer(palette = 'Set1') +
  labs(title = "Employment", x = "", y = "") +
  theme_hor() +
  ggsave("employmentxstate.png", height = 6, width = 8, dpi = 300)

ggplot(data %>% 
         filter(str_detect(state, "Michigan|Pennsylvania|Wisconsin|Florida")) %>%
         select(state, date, low, middle, high) %>%
         pivot_longer(low:high, names_to = "level", values_to = "employment")) +
  stat_summary(aes(date, employment), fun = mean, geom = 'line', size = 1, linetype = 2) +
  geom_line(aes(date, employment, group = level, colour = level), size = 1) +
  scale_colour_brewer(palette = 'Set1', name = "Income\nLevel") +
  facet_wrap(~ state) + 
  labs(title = "Employment", x = "", y = "") +
  theme_hor() +
  ggsave("employmentxlevel.png", height = 6, width = 8, dpi = 300)

