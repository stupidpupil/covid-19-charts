library("tidyverse")
library("cowplot")

max_cases = 6000
pretty_max_date = cases_by_nhs$Date %>% max %>% strftime("%d-%b-%Y")

my_plot <- ggplot(cases_by_nhs, aes(x=DaysSinceMinCases, y=Cases, group=`Health Board/NHS Region`)) +



theme_classic()+
theme(
  strip.text.x = element_text(colour='#005EB8', hjust=0.05, vjust=0, family="Bahnschrift"),
  strip.background = element_blank(),
  axis.line.x = element_blank(),
  axis.line.y = element_blank(),
  axis.text.x = element_text(colour='grey60', size=7, family="Menlo"),
  axis.text.y = element_text(colour='grey60', size=7, family="Menlo"),
  axis.title.x = element_blank(),
  axis.title.y = element_text(colour='grey60', size=8, family="Menlo", margin=margin(0.1,0.35,0.1,0.1, "cm")),
  axis.ticks.y = element_line(colour='grey60'),
  axis.ticks.x = element_line(colour='grey60'),
  plot.title = element_text(colour='grey30', size=11.5, vjust=0, family="Bahnschrift", margin=margin(0.2,0.1,0.2,0.1, "cm"), lineheight=1.1)
  )+

# All lines in the background, light grey
geom_line(data=cases_by_nhs %>% rename(TempGroup=`Health Board/NHS Region`), aes(group=TempGroup), colour='grey90', size=1.2) + 


# Fake axes
geom_hline(yintercept=min_cases, colour="grey70")+
geom_vline(xintercept=0, colour="grey70")+


# Dashed guidelines for different rates
geom_line(data=tibble(DaysSinceMinCases=c(0,14), Cases=c(min_cases, min_cases*(1.41**13))), aes(group=NULL), colour="grey70", linetype="longdash")+
geom_line(data=tibble(DaysSinceMinCases=c(0,20), Cases=c(min_cases, min_cases*(1.26**19))), aes(group=NULL), colour="grey70", linetype="longdash")+
geom_line(data=tibble(DaysSinceMinCases=c(0,20), Cases=c(min_cases, min_cases*(1.19**19))), aes(group=NULL), colour="grey70", linetype="longdash")+


# The actual region's line
geom_line(colour='#005EB8', size=0.65) +
geom_point(colour='#005EB8', size=0.3) +


scale_y_log10(limits=c(min_cases, max_cases))+
labs(
  title = paste0(
    "Cumulative confirmed COVID-19 cases in Welsh Health Boards and English NHS Regions, ",
    "\nby days-since-", min_cases, "th-case, up to ", pretty_max_date),
  y = "Confimed cases of COVID-19", 
  x = paste0("Days since ", min_cases, " confirmed cases"))+
facet_wrap(~`Health Board/NHS Region`, ncol=4, nrow=4)


my_plot <- ggdraw(my_plot) + draw_label(x=0.34, y=0.215, hjust=0, vjust=1, fontfamily="Menlo", colour='grey40', size=7.6, lineheight=1.1,
  paste0(
    "X-axis shows days since 50th confirmed case in each region.",
    "\n\n",
    "Dashed lines show rates that would produce doubling of cases \nat every 2, 3 and 4 days (top, middle, bottom).",
    "\n\n",
    "Data from Public Health Wales (collated by @LloydCymru) \nand Public Health England, as published daily, up to ", pretty_max_date, ".",
    "\n\n",
    "Chart produced by @agcwatkins, based on @jburnmurdoch's designs.",
    "\n",
    "Numbers of confirmed cases are affected by many factors."
  ))

print(my_plot)

scaling_factor = 1.56
ggsave(paste0(pretty_max_date, ".png"), height=11/scaling_factor, width=11.5/scaling_factor, dpi=72*scaling_factor)