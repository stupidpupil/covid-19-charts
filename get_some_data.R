library('tidyverse')
library('lubridate')


cases_by_nhs <- read_csv("https://docs.google.com/spreadsheets/d/1LYQ7sz8GEpS2ClwuGuu_-EIiy5x2miycsWIVVlXrZoI/export?format=csv&id=1LYQ7sz8GEpS2ClwuGuu_-EIiy5x2miycsWIVVlXrZoI&gid=1335016048")

cases_by_nhs <- cases_by_nhs %>% 
  mutate(Date = dmy(Date)) %>%
  pivot_longer(-Date, names_to = "Health Board/NHS Region", values_to = "Cases")

orgs <- read_csv("https://docs.google.com/spreadsheets/d/1LYQ7sz8GEpS2ClwuGuu_-EIiy5x2miycsWIVVlXrZoI/export?format=csv&id=1LYQ7sz8GEpS2ClwuGuu_-EIiy5x2miycsWIVVlXrZoI&gid=1236221803")

min_cases = 50

cases_by_nhs <- cases_by_nhs %>%
  group_by(`Health Board/NHS Region`) %>%
  mutate(
    DaysSinceMinCases = case_when(
        Cases < min_cases ~ -1,
        TRUE ~ 0
      )
    ) %>%
  filter(DaysSinceMinCases >= 0, !is.na(Cases)) %>%
  mutate(
    DaysSinceMinCases = as.numeric(Date-min(Date), unit="days")
    ) %>%
  ungroup()

cases_by_nhs <- cases_by_nhs %>% left_join(orgs, by="Health Board/NHS Region")

cases_by_nhs$`Health Board/NHS Region` = 
  factor(cases_by_nhs$`Health Board/NHS Region`, levels = orgs$`Health Board/NHS Region`)


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
    DaysSinceMinDeaths = case_when(
        Deaths < min_deaths ~ -1,
        TRUE ~ 0
      )
    ) %>%
  filter(DaysSinceMinDeaths >= 0, !is.na(Deaths)) %>%
  mutate(
    DaysSinceMinDeaths = as.numeric(Date-min(Date), unit="days")
    ) %>%
  ungroup()