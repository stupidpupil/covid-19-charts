library("tidyverse")
library("cowplot")

source("draw_a_chart_function.R")


my_plot <- for_new_deaths_in_last_week_chart %>% draw_a_jburnish_chart(
  x=DaysSinceMinNewDeathsInLastWeek, y=NewDeathsInLastWeek, group=Country, y_min=min_new_deaths_in_last_week, y_max = 500,
  title = paste0("New reported COVID-19 deaths in previous week, \nby days-since-", min_new_deaths_in_last_week, "-new-deaths-first-reported-in-a-week"),
  y_title = "New reported COVID-19 deaths in previous 7 days",
  x_title = paste0("Days since first ", min_cases, " deaths reported in previous week"),
  primary_colour = '#DA291C',
  draft = TRUE
  )


print(my_plot)


scaling_factor = 1.56
ggsave(paste0(pretty_max_date, " NewDeathsInLastWeek.png"), height=11/(1.8*scaling_factor), width=11.5/scaling_factor, dpi=72*scaling_factor)