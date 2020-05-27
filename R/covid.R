library(tidyverse)
library(lubridate)
library(tidyquant)

metro <- c("Bucks", "Chester", "Delaware", "Montgomery", "Philadelphia", "Burlington", "Camden", "Gloucester", "New Castle")

p1 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "New Jersey" | state == "Pennsylvania" | state == "Delaware") %>%
  filter(county %in% metro) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(multiplier = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, multiplier)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  theme_bw()

p2 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "New Jersey" | state == "Pennsylvania" | state == "Delaware") %>%
  filter(county %in% metro) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, change)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  theme_bw()

p3 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "New Jersey" | state == "Pennsylvania" | state == "Delaware") %>%
  filter(county %in% metro) %>%
  group_by(date, county) %>%
  summarise(cases = sum(cases)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, cases, fill = county)) +
  geom_bar(stat = 'identity', position = 'stack') +
  theme_bw()

library(patchwork)

p <- p1 + p2 + p3

ggsave("region.png", height = 6, width = 18, dpi = 300)

##

display.brewer.pal(3, 'Set1')
brewer.pal(3, 'Set1')

##

p1 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Michigan") %>%
  filter(county == "Macomb") %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change), 
         change_1 = lag(change_lag),
         change_2 = lag(change_1),
         change_3 = lag(change_2)) %>%
  mutate(avg = (change_lag + change_1 + change_2 + change_3) / 4) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, change)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  geom_line(aes(date, avg), colour = "#4DAF4A", size = 2) +
  labs(title = "Change (Macomb)") +
  theme_hor()

p2 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Michigan") %>%
  filter(county == "Macomb") %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, cases)) +
  geom_bar(stat = 'identity') +
  labs(title = "Absolute (Macomb)") +
  theme_hor()

p3 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Wisconsin") %>%
  filter(str_detect(county, "Racine|Kenosha")) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change), 
         change_1 = lag(change_lag),
         change_2 = lag(change_1),
         change_3 = lag(change_2)) %>%
  mutate(avg = (change_lag + change_1 + change_2 + change_3) / 4) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, change)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  geom_line(aes(date, avg), colour = "#4DAF4A", size = 2) +
  labs(title = "Change (Racine/Kenosha)") +
  theme_hor()

p4 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Wisconsin") %>%
  filter(str_detect(county, "Racine|Kenosha")) %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, cases)) +
  geom_bar(stat = 'identity') +
  labs(title = "Absolute (Race/Kenosha)") +
  theme_hor()

p5 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Pennsylvania") %>%
  filter(county == "Luzerne") %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change), 
         change_1 = lag(change_lag),
         change_2 = lag(change_1),
         change_3 = lag(change_2)) %>%
  mutate(avg = (change_lag + change_1 + change_2 + change_3) / 4) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, change)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  geom_line(aes(date, avg), colour = "#4DAF4A", size = 2) +
  labs(title = "Change (Luzerne)") +
  theme_hor()

p6 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Pennsylvania") %>%
  filter(county == "Luzerne") %>%
  group_by(date, county) %>%
  summarise(cases = sum(cases)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, cases)) +
  geom_bar(stat = 'identity') +
  labs(title = "Absolute (Luzerne)") +
  theme_hor()

p7 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Florida") %>%
  filter(county == "Pinellas") %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change), 
         change_1 = lag(change_lag),
         change_2 = lag(change_1),
         change_3 = lag(change_2)) %>%
  mutate(avg = (change_lag + change_1 + change_2 + change_3) / 4) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, change)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  geom_line(aes(date, avg), colour = "#4DAF4A", size = 2) +
  labs(title = "Change (Pinellas)") +
  theme_hor()

p8 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Florida") %>%
  filter(county == "Pinellas") %>%
  group_by(date, county) %>%
  summarise(cases = sum(cases)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, cases)) +
  geom_bar(stat = 'identity') +
  labs(title = "Absolute (Pinellas)") +
  theme_hor()

p9 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Ohio") %>%
  filter(county == "Montgomery") %>%
  group_by(date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change), 
         change_1 = lag(change_lag),
         change_2 = lag(change_1),
         change_3 = lag(change_2)) %>%
  mutate(avg = (change_lag + change_1 + change_2 + change_3) / 4) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, change)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  geom_line(aes(date, avg), colour = "#4DAF4A", size = 2) +
  labs(title = "Change (Montgomery)") +
  theme_hor()

p0 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Ohio") %>%
  filter(county == "Montgomery") %>%
  group_by(date, county) %>%
  summarise(cases = sum(cases)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, cases)) +
  geom_bar(stat = 'identity') +
  labs(title = "Absolute (Montgomery)") +
  theme_hor()

library(patchwork)

pm <- p1 + p2 
pr <- p3 + p4 
pl <- p5 + p6
pp <- p7 + p8
po <- p9 + p0

p <- pm / pr / pl / pp + po + plot_annotation(
  title = 'CASES'
)

ggsave("counties_cases.png", height = 22, width = 18, dpi = 300)

##

p1 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Michigan") %>%
  filter(county == "Macomb") %>%
  group_by(date) %>%
  summarise(cases = sum(deaths)) %>%
  ungroup() %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change), 
         change_1 = lag(change_lag),
         change_2 = lag(change_1),
         change_3 = lag(change_2)) %>%
  mutate(avg = (change_lag + change_1 + change_2 + change_3) / 4) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, change)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  geom_line(aes(date, avg), colour = "#4DAF4A", size = 2) +
  labs(title = "Change (Macomb)") +
  theme_hor()

p2 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Michigan") %>%
  filter(county == "Macomb") %>%
  group_by(date) %>%
  summarise(cases = sum(deaths)) %>%
  ungroup() %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, cases)) +
  geom_bar(stat = 'identity') +
  labs(title = "Absolute (Macomb)") +
  theme_hor()

p3 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Wisconsin") %>%
  filter(str_detect(county, "Racine|Kenosha")) %>%
  group_by(date) %>%
  summarise(cases = sum(deaths)) %>%
  ungroup() %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change), 
         change_1 = lag(change_lag),
         change_2 = lag(change_1),
         change_3 = lag(change_2)) %>%
  mutate(avg = (change_lag + change_1 + change_2 + change_3) / 4) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, change)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  geom_line(aes(date, avg), colour = "#4DAF4A", size = 2) +
  labs(title = "Change (Racine/Kenosha)") +
  theme_hor()

p4 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Wisconsin") %>%
  filter(str_detect(county, "Racine|Kenosha")) %>%
  group_by(date) %>%
  summarise(cases = sum(deaths)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, cases)) +
  geom_bar(stat = 'identity') +
  labs(title = "Absolute (Race/Kenosha)") +
  theme_hor()

p5 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Pennsylvania") %>%
  filter(county == "Luzerne") %>%
  group_by(date) %>%
  summarise(cases = sum(deaths)) %>%
  ungroup() %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change), 
         change_1 = lag(change_lag),
         change_2 = lag(change_1),
         change_3 = lag(change_2)) %>%
  mutate(avg = (change_lag + change_1 + change_2 + change_3) / 4) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, change)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  geom_line(aes(date, avg), colour = "#4DAF4A", size = 2) +
  labs(title = "Change (Luzerne)") +
  theme_hor()

p6 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Pennsylvania") %>%
  filter(county == "Luzerne") %>%
  group_by(date, county) %>%
  summarise(cases = sum(deaths)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, cases)) +
  geom_bar(stat = 'identity') +
  labs(title = "Absolute (Luzerne)") +
  theme_hor()

p7 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Florida") %>%
  filter(county == "Pinellas") %>%
  group_by(date) %>%
  summarise(cases = sum(deaths)) %>%
  ungroup() %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change), 
         change_1 = lag(change_lag),
         change_2 = lag(change_1),
         change_3 = lag(change_2)) %>%
  mutate(avg = (change_lag + change_1 + change_2 + change_3) / 4) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, change)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  geom_line(aes(date, avg), colour = "#4DAF4A", size = 2) +
  labs(title = "Change (Pinellas)") +
  theme_hor()

p8 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Florida") %>%
  filter(county == "Pinellas") %>%
  group_by(date, county) %>%
  summarise(cases = sum(deaths)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, cases)) +
  geom_bar(stat = 'identity') +
  labs(title = "Absolute (Pinellas)") +
  theme_hor()

p9 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Ohio") %>%
  filter(county == "Montgomery") %>%
  group_by(date) %>%
  summarise(cases = sum(deaths)) %>%
  ungroup() %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change), 
         change_1 = lag(change_lag),
         change_2 = lag(change_1),
         change_3 = lag(change_2)) %>%
  mutate(avg = (change_lag + change_1 + change_2 + change_3) / 4) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, change)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  geom_line(aes(date, avg), colour = "#4DAF4A", size = 2) +
  labs(title = "Change (Montgomery)") +
  theme_hor()

p0 <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv") %>%
  filter(date > as_date('2020-02-29')) %>%
  filter(state == "Ohio") %>%
  filter(county == "Montgomery") %>%
  group_by(date, county) %>%
  summarise(cases = sum(deaths)) %>%
  mutate(cases_lag = lag(cases)) %>%
  mutate(change = cases - cases_lag) %>%
  mutate(change_lag = lag(change)) %>%
  mutate(coefficient = change / change_lag) %>%
  arrange(desc(date)) %>%
  ggplot(aes(date, cases)) +
  geom_bar(stat = 'identity') +
  labs(title = "Absolute (Montgomery)") +
  theme_hor()

library(patchwork)

pm <- p1 + p2 
pr <- p3 + p4 
pl <- p5 + p6
pp <- p7 + p8
po <- p9 + p0

p <- pm / pr / pl / pp + po + plot_annotation(
  title = 'DEATHS'
)

ggsave("counties_death.png", height = 22, width = 18, dpi = 300)

