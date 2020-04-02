draw_a_jburnish_chart <- function(
  data_for_chart,
  x,
  y,
  group,
  title,
  y_title,
  x_title,
  y_min = 10, # HACK?
  y_max = 10000, # HACK
  primary_colour = '#005EB8', # NHS Blue
  draft = FALSE,
  draw_points = TRUE,
  guides_doubling_every_x_days = c(2,3,4)
  ){


  pretty_max_date = data_for_chart$Date %>% max %>% strftime("%d-%b-%Y")

  my_plot <- ggplot(data_for_chart, aes(x=!!enquo(x), y=!!enquo(y), group=!!enquo(group))) +

    scale_y_log10(limits=c(y_min, y_max)) +
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
    geom_line(data=data_for_chart %>% rename(TempGroup=(!!enquo(group))), aes(group=TempGroup), colour='grey90', size=1.2) + 


    # Fake axes
    geom_hline(yintercept=y_min, colour="grey70")+
    geom_vline(xintercept=0, colour="grey70")


  # Guide
  for (x_days in guides_doubling_every_x_days) {

    q <- 2**(1/x_days)

    guide_data <- tibble(!!enquo(x) := seq(0, data_for_chart %>% pull(!!enquo(x)) %>% max())) %>%
      mutate(!!enquo(y) := y_min*(q**(!!enquo(x))))
    
    my_plot <- my_plot + geom_line(data=guide_data, aes(group=NULL), colour="grey70", linetype="longdash")

  }

  # The actual region's line
  my_plot <- my_plot + geom_line(colour=primary_colour, size=0.65)
    

  if(draw_points){
    my_plot <-  my_plot + geom_point(colour=primary_colour, size=0.3)
  }





  my_plot <- my_plot +
    labs(
      title = paste0(title,", up to ", pretty_max_date),
    y = y_title, x = title)

  
  my_plot <- my_plot + facet_wrap(as.formula(paste0("~",substitute(group))), ncol=4, nrow=4)


  my_plot <- ggdraw(my_plot)

  if(draft){
    my_plot <- my_plot + draw_label(fontfamily="Menlo", colour='grey40', size=120, alpha=0.2, angle=45, "DRAFT")    
  }


  my_plot
}
