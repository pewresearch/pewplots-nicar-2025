install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)

#create parameters to specify and their defaults.
#if style guidance makes any of these irrelevant to you, 
#delete them and/or specify the default behavior yourself!
my_theme <- function(legend = FALSE, axes = FALSE, gridlines = FALSE, ...){
  
  #base font size, in pts
  base_size = 8
  #base font family - what font does your organization use?
  base_family = "___"
  
  #headline
  plot_title <- element_text(family = "___",
                             size = 13,
                             margin =  margin(b=4, unit = "pt"),
                             #left aligned; for center, hjust = 0.5
                             hjust = 0, vjust = 0)
  #subheadline
  plot_subtitle <- element_text(family = "___",
                                size = 9,
                                #italic, bold?
                                face = "___",
                                margin =  margin(b = 6, unit = "pt"),
                                #left aligned; for center, hjust = 0.5
                                hjust = 0, vjust = 0)
  
  #facet formatting
  strip_background <- element_blank()
  strip_text <- element_text(family = "___", size = 8.5)
  

  #keep the conditional statements for each element separate!
  #starting with legend, TRUE or FALSE: 
  if (legend == TRUE) {
    legend_title <-  element_blank()
    legend_margin <-  margin(0,0,0,0)
    legend_position <- "top" #or "bottom", "left", "right"
    legend_box_margin <-  margin(-5,-10, 0,-10) 
    legend_text <- element_text()
  } else {
    legend_position <- "none"
    legend_title <-  element_blank()
    legend_text <-  element_blank()
    legend_margin <- NULL
    legend_box_margin <- NULL
  }
  
  #axes - also TRUE or FALSE
  if (axes == TRUE) {
    axis_line <- element_line(color = "#A4A4A4", linewidth = .2)
    axis_line_y <- element_blank() #if you want a y-axis line, delete this and line 62
    axis_ticks_x <-  element_line(color = "#A4A4A4", linewidth = .2)
    axis_ticks_y <- element_line(linewidth = 0) #if you want y-axis ticks, delete this and line 65
    axis_ticks_length <- unit(6, "pt")
    axis_text_x <- element_text(family = base_family,  size = 8, vjust = 0)
    axis_text_y <- element_text(family = base_family, size = 8, hjust = 1, vjust = 0.5)
    axis_title <- element_blank() #replace with element_text() if axes need labels
    panel_border =  element_blank()
  } else {
    axis_line<- element_line(linewidth = 0)
    axis_line_y <- NULL
    axis_ticks<-element_line(linewidth = 0)
    axis_ticks_x <-element_line(linewidth = 0)
    axis_ticks_y <-element_line(linewidth = 0)
    axis_ticks_length <- unit(0, "pt")
    axis_text_x<-element_blank()
    axis_text_y<-element_text(family = base_family, size = 8, hjust = 1, vjust = 0.5)
    axis_title<-element_blank()
    panel_border <- element_blank()
  }
  
  #gridlines: one set, two sets, or none (FALSE)?
  #linetype: dotted or solid?
  #linewidth: how thick should the lines be?
  #color: can provide a hexcode or color name
  if (gridlines == 1) { #no x gridlines, yes y gridlines
    panel_grid_major_x<-element_blank()
    panel_grid_minor_y<-element_blank()
    panel_grid_minor_x<-element_blank()
    panel_grid<-element_line(color = "black", linewidth = .2)
    panel_grid_major_y<-element_line(linetype = "dotted", color = "black", linewidth = .3)
  } else if (gridlines == 2) { #both x and y gridlines
    panel_grid<-element_line(color = "black", linewidth = .2)
    panel_grid_major_y<-element_line(linetype = "solid", color = "#D7D7D7", linewidth = .1)
    panel_grid_major_x<-element_line(linetype = "dotted", color = "black", linewidth = .3)
    panel_grid_minor_y<-element_blank()
    panel_grid_minor_x<-element_blank()
  } else { #no gridlines
    panel_grid <- element_blank()
    panel_grid_major_y <- NULL
    panel_grid_major_x <- NULL
    panel_grid_minor_y <- NULL
    panel_grid_minor_x <- NULL
  }
  
  #now, just plug in the objects you created:
  theme <- theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(plot.title = plot_title,
          plot.subtitle = plot_subtitle,
          plot.caption = plot_caption,
          legend.position = legend_position,
          legend.title = legend_title,
          legend.text = legend_text,
          legend.margin = legend_margin,
          legend.box.margin = legend_box_margin,
          axis.line = axis_line,
          axis.line.y = axis_line_y,
          axis.ticks.x =  axis_ticks_x,
          axis.ticks.y =  axis_ticks_y,
          axis.ticks.length =  axis_ticks_length,
          axis.text.x = axis_text_x,
          axis.text.y = axis_text_y,
          axis.title =  axis_title,
          panel.grid =  panel_grid,
          panel.grid.major.y =  panel_grid_major_y,
          panel.grid.major.x =   panel_grid_major_x,
          panel.grid.minor.y =   panel_grid_minor_y,
          panel.grid.minor.x =  panel_grid_minor_x,
          panel.border =  panel_border,
          strip.background =  strip_background,
          strip.text = strip_text,
          complete = TRUE
    )
}