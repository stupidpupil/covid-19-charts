library("tidyverse")
library("cowplot")

source("draw_a_chart_function.R")

my_plot <- for_min_cases_chart %>% draw_a_jburnish_chart(
  x=DaysSinceMinCases, y=Cases, group=Region, y_min=min_cases,
  title = paste0("Cumulative confirmed COVID-19 cases in Welsh Health Boards and English NHS Regions, \nby days-since-", min_cases, "th-case"),
  y_title = "Cumulative confirmed cases of COVID-19",
  x_title = paste0("Days since ", min_cases, " confirmed cases")
  )

my_plot <- ggdraw(my_plot) + draw_label(x=0.34, y=0.215, hjust=0, vjust=1, fontfamily="Menlo", colour='grey40', size=7.6, lineheight=1.1,
  paste0(
    "X-axis shows days since 50th confirmed case in each region.",
    "\n\n",
    "Dashed lines show rates that would produce doubling of cases \nat every 2, 3 and 4 days (top, middle, bottom).",
    "\n\n",
    "Data from Public Health Wales",
    #"(collated by @LloydCymru) ",
    "\nand Public Health England, as published daily, up to ", pretty_max_date, ".",
    "\n\n",
    #"Chart produced by @agcwatkins, based on @jburnmurdoch's designs.",
    "\n",
    "Numbers of confirmed cases are affected by many factors."
  ))


print(my_plot)


scaling_factor = 1.56
ggsave(
  paste0("output/", pretty_max_date," Cumulative Cases", ".png"), 
  height=11/scaling_factor, width=11.5/scaling_factor, dpi=72*scaling_factor)