library("tidyverse")
library("cowplot")

source("draw_a_chart_function.R")

my_plot <- for_new_cases_in_last_week_chart %>% draw_a_jburnish_chart(
  x = DaysSinceMinNewCasesInLastWeek, y = NewCasesInLastWeek, group = Region, y_min = min_new_cases_in_last_week, y_max = 5100,
  title = paste0("New confirmed COVID-19 cases in previous week, \nby days-since-", min_new_cases_in_last_week, "-new-cases-first-confirmed-in-a-week"),
  y_title = "New confirmed cases of COVID-19 in previous 7 days",
  x_title = paste0("Days since first ", min_cases, " cases confirmed in previous week"),
  primary_colour = "#330072", # Purple
  draft = TRUE
)

print(my_plot)

scaling_factor <- 1.56
ggsave(
  paste0("output/", pretty_max_date, " New Cases in Last Week", ".png"),
  height = 11 / (1.1 * scaling_factor), width = 11.5 / scaling_factor, dpi = 72 * scaling_factor
)
