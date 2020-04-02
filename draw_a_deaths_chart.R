library("tidyverse")
library("cowplot")

max_deaths = 3000
pretty_max_date = for_deaths_chart$Date %>% max %>% strftime("%d-%b-%Y")

my_plot <- ggplot(for_deaths_chart, aes(x=DaysSinceMinDeaths, y=Deaths, group=Country)) +



theme_classic()+
theme(
  strip.text.x = element_text(colour='#F28E00', hjust=0.05, vjust=0, family="Bahnschrift"),
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
geom_line(data=for_deaths_chart %>% rename(TempGroup=Country), aes(group=TempGroup), colour='grey90', size=1.2) + 


# Fake axes
geom_hline(yintercept=min_deaths, colour="grey70")+
geom_vline(xintercept=0, colour="grey70")+


# Dashed guidelines for different rates
geom_line(data=tibble(DaysSinceMinDeaths=c(0,14), Deaths=c(min_deaths, min_deaths*(1.41**13))), aes(group=NULL), colour="grey70", linetype="longdash")+
geom_line(data=tibble(DaysSinceMinDeaths=c(0,19), Deaths=c(min_deaths, min_deaths*(1.26**18))), aes(group=NULL), colour="grey70", linetype="longdash")+
geom_line(data=tibble(DaysSinceMinDeaths=c(0,20), Deaths=c(min_deaths, min_deaths*(1.19**19))), aes(group=NULL), colour="grey70", linetype="longdash")+


# The actual region's line
geom_line(colour='#F28E00', size=0.65) +
geom_point(colour='#F28E00', size=0.3) +



scale_y_log10(limits=c(min_deaths, max_deaths))+
labs(
  title = paste0(
    "Cumulative reported deaths with COVID-19 in countries of UK, and four other countries, ",
    "\nby days-since-", min_deaths, "th-death, up to ", pretty_max_date),
  y = "Reported deaths with COVID-19", 
  x = paste0("Days since ", min_deaths, " deaths"))+
facet_wrap(~Country, ncol=4, nrow=4)


#my_plot <- ggdraw(my_plot) + draw_label(x=0.34, y=0.215, hjust=0, vjust=1, fontfamily="Menlo", colour='grey40', size=7.6, lineheight=1.1,
#  paste0(
#    "X-axis shows days since 20th confirmed death in each region.",
#    "\n\n",
#    "Dashed lines show rates that would produce doubling of Deaths \nat every 2, 3 and 4 days (top, middle, bottom).",
#    "\n\n",
#    "Data from Public Health Wales",
#    #"(collated by @LloydCymru) ",
#    "\nand Public Health England, as published daily, up to ", pretty_max_date, ".",
#    "\n\n",
#    #"Chart produced by @agcwatkins, based on @jburnmurdoch's designs.",
#    "\n",
#    "Numbers of deaths are affected by many factors."
#  ))

#my_plot <- ggdraw(my_plot) + draw_label("Draft", color = "grey30", size = 120, angle=45, alpha=0.05)


print(my_plot)

scaling_factor = 1.56
ggsave(paste0(pretty_max_date, " Deaths", ".png"), height=11/(1.6*scaling_factor), width=11.5/scaling_factor, dpi=72*scaling_factor)
