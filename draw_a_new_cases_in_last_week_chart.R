library("tidyverse")
library("cowplot")

max_cases = 5100
primary_colour = '#005EB8'


pretty_max_date = for_new_cases_in_last_week_chart$Date %>% max %>% strftime("%d-%b-%Y")



my_plot <- ggplot(for_new_cases_in_last_week_chart, aes(x=DaysSinceMinNewCasesInLastWeek, y=NewCasesInLastWeek, group=Region)) +


theme_classic()+
theme(
  strip.text.x = element_text(colour=primary_colour, hjust=0.05, vjust=0, family="Bahnschrift"),
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
geom_line(data=for_new_cases_in_last_week_chart %>% rename(TempGroup=Region), aes(group=TempGroup), colour='grey90', size=1.2) + 


# Fake axes
geom_hline(yintercept=min_new_cases_in_last_week, colour="grey70")+
geom_vline(xintercept=0, colour="grey70")+



# The actual region's line
geom_line(colour=primary_colour, size=0.65) +
geom_point(colour=primary_colour, size=0.3) +



scale_y_log10(limits=c(min_new_cases_in_last_week, max_cases))+
labs(
  title = paste0(
    "New confirmed COVID-19 cases in previous week, ",
    "\nby days-since-", min_new_cases_in_last_week, "-new-cases-first-recorded-in-a-week, up to ", pretty_max_date),
  y = "New confirmed cases of COVID-19 in previous 7 days", 
  x = paste0("Days since first ", min_new_cases_in_last_week, " confirmed cases in previous week"))+
facet_wrap(~Region, ncol=4, nrow=4)


my_plot <- ggdraw(my_plot) + draw_label(fontfamily="Menlo", colour='grey40', size=120, alpha=0.2, angle=45, "DRAFT")

print(my_plot)




scaling_factor = 1.56
ggsave(paste0(pretty_max_date, " NewInLastWeek.png"), height=11/(1.1*scaling_factor), width=11.5/scaling_factor, dpi=72*scaling_factor)