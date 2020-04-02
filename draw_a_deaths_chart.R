library("tidyverse")
library("cowplot")

source("draw_a_chart_function.R")


my_plot <- for_deaths_chart %>% draw_a_jburnish_chart(
  x=DaysSinceMinDeaths, y=Deaths, group=Country, y_min=min_deaths, y_max = 3000, 
  title = paste0("Cumulative reported deaths with COVID-19 in countries of UK, and four other countries, \nby days-since-", min_deaths, "th-death"),
  y_title = "Reported deaths with COVID-19",
  x_title = paste0("Days since ", min_deaths, " deaths"),
  primary_colour = '#F28E00'
  )


print(my_plot)

scaling_factor = 1.56
ggsave(paste0(pretty_max_date, " Deaths", ".png"), height=11/(1.6*scaling_factor), width=11.5/scaling_factor, dpi=72*scaling_factor)
