library("tidyverse")
library("lubridate")

source("rolling_seven_day_sums.R")
source("days_since_min.R")

workbook_url = "https://docs.google.com/spreadsheets/d/1LYQ7sz8GEpS2ClwuGuu_-EIiy5x2miycsWIVVlXrZoI/export?format=csv&id=1LYQ7sz8GEpS2ClwuGuu_-EIiy5x2miycsWIVVlXrZoI"

cases_by_nhs <- read_csv(paste0(workbook_url, "&gid=1335016048"))

cases_by_nhs <- cases_by_nhs %>%
  mutate(Date = dmy(Date)) %>%
  pivot_longer(-Date, names_to = "Region", values_to = "Cases")

pretty_max_date <- cases_by_nhs$Date %>%
  max() %>%
  strftime("%d-%b-%Y")
  

orgs <- read_csv(paste0(workbook_url, "&gid=1236221803"))

cases_by_nhs$Region <-
  factor(cases_by_nhs$Region, levels = orgs$Region)


cases_by_nhs <- cases_by_nhs %>%
  filter(!is.na(Cases)) %>% # Otherwise you have an issue with missing rows on 18-Mar
  group_by(Region) %>%
  arrange(Date) %>%
  mutate(NewCases = ifelse(is.na(lag(Cases)), Cases, Cases - lag(Cases))) %>%
  ungroup()

#
# Cumulative Cases
#

min_cases <- 50

for_min_cases_chart <- cases_by_nhs %>%
  add_days_since_min(Cases, group = Region, min = min_cases)


#
# New Cases in last Week
#

min_new_cases_in_last_week <- 100

for_new_cases_in_last_week_chart <- cases_by_nhs %>%
  add_rolling_seven_day_sums(NewCases, group = Region) %>%
  add_days_since_min(NewCasesInLastWeek, group = Region, min = min_new_cases_in_last_week)


#
# Deaths
#

deaths_by_country <- read_csv(paste0(workbook_url, "&gid=2034844950"))

deaths_by_country <- deaths_by_country %>%
  mutate(Date = dmy(Date)) %>%
  pivot_longer(-Date, names_to = "Country", values_to = "Deaths")

deaths_by_country <- deaths_by_country %>% filter(Country != "UK")

deaths_by_country$Country <- factor(deaths_by_country$Country, levels = c(
  "Wales", "Scotland", "Northern Ireland", "England",
  "Denmark", "Germany", "Italy", "Spain"
))

deaths_by_country <- deaths_by_country %>%
  group_by(Country) %>%
  arrange(Date) %>%
  mutate(NewDeaths = ifelse(is.na(lag(Deaths)), Deaths, Deaths - lag(Deaths))) %>%
  ungroup()


#
# Cumulative deaths
#

min_deaths <- 20

for_deaths_chart <- deaths_by_country %>%
  add_days_since_min(Deaths, group = Country, min = min_deaths)


#
# New deaths in last week
#

min_new_deaths_in_last_week <- 10

for_new_deaths_in_last_week_chart <- deaths_by_country %>%
  add_rolling_seven_day_sums(NewDeaths, group = Country) %>%
  add_days_since_min(NewDeathsInLastWeek, group = Country, min = min_new_deaths_in_last_week)
