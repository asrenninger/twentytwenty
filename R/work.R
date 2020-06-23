library(tidyverse)
library(glue)
library(janitor)
library(lubridate)

cleaner <- function(x) { as.numeric(str_replace_all(x, pattern = "%", replacement = ""))}

hours_state <- 
  read_csv("data/homebase/COVID-19 impact on US Local Businesses and Hourly Employees - 1. Hours worked by hourly employees.csv", 
         skip = 72) %>%
  clean_names() %>%
  select(-x1, -x2) %>%
  rename(STATE = x3) %>%
  drop_na(STATE) %>%
  mutate_at(vars(2:ncol(.)), cleaner) %>%
  pivot_longer(cols = x5_13:x3_1, names_to = "date") %>%
  mutate(date = str_replace_all(date, pattern = "x", replacement = ""),
         date = str_replace_all(date, pattern = "_", replacement = "-"),
         date = glue("2020-{date}"), 
         date = as_date(date)) %>%
  group_by(STATE) %>%
  mutate(hours_lag1 = lag(value)) %>%
  mutate(hours_lag2 = lag(hours_lag1), 
         hours_lag3 = lag(hours_lag2),
         hours_lag4 = lag(hours_lag3),
         hours_lag5 = lag(hours_lag4)) %>%
  mutate(rolling_avg = (hours_lag1 + hours_lag2 + hours_lag3 + hours_lag4 + hours_lag5) / 5) %>%
  select(-hours_lag1, -hours_lag2, -hours_lag3, -hours_lag4,  -hours_lag5)
  
brewer.pal(3, "Set1")  

theme_ver <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.y = element_line(size = 0.5, colour = 'black'),
          axis.line.x = element_blank(),
          axis.ticks.y = element_line(size = 0.5, colour = 'black'),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(face = 'bold'),
          axis.text.x = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(20, 20, 20, 20)
    )
}

ggplot(hours_state %>%
         filter(str_detect(STATE, "Pennsylvania|Florida|Wisconsin|Michigan")),
       aes(date, rolling_avg)) +
  geom_line(size = 0.74, colour = '#E41A1C') +
  facet_wrap(~ STATE) +
  xlab("") +
  ylab("% change in hours worked (from seasonal baseline)") +
  theme_ver() +
  ggsave("hours.png", height = 6, width = 10, dpi = 300)

hours_msa <- 
  read_csv("data/homebase/COVID-19 impact on US Local Businesses and Hourly Employees - 1. Hours worked by hourly employees.csv", 
           skip = 6) %>%
  clean_names() %>%
  select(-x1, -x2) %>%
  rename(MSA = x3) %>%
  drop_na(MSA) %>%
  mutate_at(vars(2:ncol(.)), cleaner) %>%
  pivot_longer(cols = x5_13:x3_1, names_to = "date") %>%
  mutate(date = str_replace_all(date, pattern = "x", replacement = ""),
         date = str_replace_all(date, pattern = "_", replacement = "-"),
         date = glue("2020-{date}"), 
         date = as_date(date)) %>%
  group_by(MSA) %>%
  mutate(hours_lag1 = lag(value)) %>%
  mutate(hours_lag2 = lag(hours_lag1), 
         hours_lag3 = lag(hours_lag2),
         hours_lag4 = lag(hours_lag3),
         hours_lag5 = lag(hours_lag4)) %>%
  mutate(rolling_avg = (hours_lag1 + hours_lag2 + hours_lag3 + hours_lag4 + hours_lag5) / 5) %>%
  select(-hours_lag1, -hours_lag2, -hours_lag3, -hours_lag4,  -hours_lag5)


ggplot(hours_msa %>%
         filter(str_detect(STATE, "New York|Philadelphia|Ohio|Wisconsin|Michigan")),
       aes(date, rolling_avg)) +
  geom_line(size = 0.74) +
  facet_wrap(~ STATE)
  

