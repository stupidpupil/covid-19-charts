library('tidyverse')
library('lubridate')
library('RcppRoll')

cases_by_nhs <- read_csv("https://docs.google.com/spreadsheets/d/1LYQ7sz8GEpS2ClwuGuu_-EIiy5x2miycsWIVVlXrZoI/export?format=csv&id=1LYQ7sz8GEpS2ClwuGuu_-EIiy5x2miycsWIVVlXrZoI&gid=1335016048")

cases_by_nhs <- cases_by_nhs %>% 
  mutate(Date = dmy(Date)) %>%
  pivot_longer(-Date, names_to = "Region", values_to = "Cases")

orgs <- read_csv("https://docs.google.com/spreadsheets/d/1LYQ7sz8GEpS2ClwuGuu_-EIiy5x2miycsWIVVlXrZoI/export?format=csv&id=1LYQ7sz8GEpS2ClwuGuu_-EIiy5x2miycsWIVVlXrZoI&gid=1236221803")


pretty_max_date = cases_by_nhs$Date %>% max %>% strftime("%d-%b-%Y")


cases_by_nhs$Region = 
  factor(cases_by_nhs$Region, levels = orgs$Region)


min_cases = 50

cases_by_nhs <- cases_by_nhs %>%
  group_by(Region) %>%
  filter(!is.na(Cases)) %>%
  arrange(Date) %>%
  mutate(
    NewCases = ifelse(is.na(lag(Cases)), Cases, Cases-lag(Cases)),
    DaysSinceMinCases = case_when(
        Cases < min_cases ~ -1,
        TRUE ~ 0
      )
    )


for_min_cases_chart <- cases_by_nhs %>% 
  filter(DaysSinceMinCases == 0) %>%
  mutate(DaysSinceMinCases = as.numeric(Date-min(Date), unit="days") ) %>%
  ungroup()


min_new_cases_in_last_week = 100

for_new_cases_in_last_week_chart <- cases_by_nhs %>% 
  ungroup() %>% complete(nesting(Region, Date)) %>% group_by(Region) %>%
  mutate(
    NewCasesInLastWeek = roll_sum(NewCases, 7, align="right", fill=NA),
    DaysSinceMinNewCasesInLastWeek = case_when(
        is.na(NewCasesInLastWeek) ~ -1,
        NewCasesInLastWeek < min_new_cases_in_last_week ~ -1,
        TRUE ~ 0
      )
  ) %>% 
  filter(DaysSinceMinNewCasesInLastWeek == 0 ) %>%
  mutate(DaysSinceMinNewCasesInLastWeek = as.numeric(Date-min(Date), unit="days") ) %>%
  ungroup()


#
# Deaths
#

min_deaths = 20

deaths_by_country <- read_csv("https://docs.google.com/spreadsheets/d/1LYQ7sz8GEpS2ClwuGuu_-EIiy5x2miycsWIVVlXrZoI/export?format=csv&id=1LYQ7sz8GEpS2ClwuGuu_-EIiy5x2miycsWIVVlXrZoI&gid=2034844950")

deaths_by_country <- deaths_by_country %>% 
  mutate(Date = dmy(Date)) %>%
  pivot_longer(-Date, names_to = "Country", values_to = "Deaths")

deaths_by_country <- deaths_by_country %>% filter(Country != 'UK')

deaths_by_country$Country <- factor(deaths_by_country$Country, levels=c(
  "Wales", "Scotland", "Northern Ireland", "England",
  "Denmark", "Germany", "Italy", "Spain"))

deaths_by_country <- deaths_by_country %>%
  group_by(Country) %>%
  mutate(
    NewDeaths = ifelse(is.na(lag(Deaths)), Deaths, Deaths-lag(Deaths)),
    DaysSinceMinDeaths = case_when(
        Deaths < min_deaths ~ -1,
        TRUE ~ 0
      )
    )


for_deaths_chart <- deaths_by_country %>%
  filter(DaysSinceMinDeaths >= 0, !is.na(Deaths)) %>%
  mutate(
    DaysSinceMinDeaths = as.numeric(Date-min(Date), unit="days")
    ) %>%
  ungroup()


min_new_deaths_in_last_week = 10

for_new_deaths_in_last_week_chart <- deaths_by_country %>% 
  ungroup() %>% complete(nesting(Country, Date)) %>% group_by(Country) %>%
  mutate(
    NewDeathsInLastWeek = roll_sum(NewDeaths, 7, align="right", fill=NA),
    DaysSinceMinNewDeathsInLastWeek = case_when(
        is.na(NewDeathsInLastWeek) ~ -1,
        NewDeathsInLastWeek < min_new_deaths_in_last_week ~ -1,
        TRUE ~ 0
      )
  ) %>% 
  filter(DaysSinceMinNewDeathsInLastWeek == 0 ) %>%
  mutate(DaysSinceMinNewDeathsInLastWeek = as.numeric(Date-min(Date), unit="days") ) %>%
  ungroup()


