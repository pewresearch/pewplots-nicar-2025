install.packages("tidyverse", dependencies = TRUE)
install.packages("magrittr", dependencies = TRUE)
install.packages("rlang", dependencies = TRUE)
install.packages("scales", dependencies = TRUE)
install.packages("lemon", dependencies = TRUE)
library(tidyverse)
library(magrittr)
library(rlang)
library(scales)
library(lemon)

my_line <- function(data,
                     x,
                     y,
                     color_by = NULL, #user provided
                     color = "___", #your default color, name or hexcode
                     facet_by = NULL, #user provided
                     facet_ncol = 1,
                     facet_scales = "___", #fixed or free
                     points = "extremes", #your default point style 
                     points_at = NULL, #user provided
                     data_labels = NULL, #user provided, or can set your own default, e.g. "all"
                     data_labels_color = FALSE, #or TRUE
                     data_labels_at = NULL, ...) {
  
  # ASSIGN VARS AND REFRAME DATA --------------------------------------------
  #the next few lines allow R to wait to evaluate an expression until later
  #more about quosures here: https://adv-r.hadley.nz/quasiquotation.html
  quo_x <- enquo(x)
  quo_y <- enquo(y)
  quo_color <- enquo(color_by)
  quo_facet <- enquo(facet_by)
  
  
  #if facetting variable provided, group data by that variable.
  if (quo_is_null(quo_facet)) {
    data <- data
  } else {
    data <- data %>%
      group_by(!!quo_facet)
  }
  
  
  #label positioning functionality for later in the function
  add_label_position <- function(df) {
    mutate(df,
           label_hjust = case_when(!!quo_x == max(!!quo_x) ~ -0.5,
                                   !!quo_x == min(!!quo_x) ~ 1.5,
                                   TRUE ~ 0),
           label_vjust = case_when(!!quo_x == max(!!quo_x) ~ 0,
                                   !!quo_x == min(!!quo_x) ~ 0,
                                   TRUE ~ -1))
  }
  
  #data and positioning for possible label positions
  if (points == "all" | points == "none" | points == FALSE) {
    points_data <- data %>%
      add_label_position()
  } else if (points == "extremes") {
    points_data <- data %>%
      filter(!!quo_x == min(!!quo_x) | !!quo_x == max(!!quo_x)) %>%
      add_label_position()
  } else if (points == "final") {
    points_data <- data %>%
      filter(!!quo_x == max(!!quo_x)) %>%
      add_label_position()
  } else if (points == "specific") {
    points_data <- data %>%
      add_label_position() %>%
      filter(as.character(!!quo_x) %in% points_at)
  }
  
  if (is.null(data_labels)) {
    data_labels <- points
  }
  
  if (data_labels == TRUE | data_labels == "none" | data_labels == FALSE) {
    labels_data <- points_data
  } else if (data_labels == "all") {
    labels_data <- data %>%
      add_label_position()
  } else if (data_labels == "extremes") {
    labels_data <- data %>%
      filter(!!quo_x == min(!!quo_x) | !!quo_x == max(!!quo_x)) %>%
      add_label_position()
  } else if (data_labels == "final") {
    labels_data <- data %>%
      filter(!!quo_x == max(!!quo_x)) %>%
      add_label_position()
  } else if (data_labels == "specific") {
    labels_data <- data %>%
      add_label_position() %>%
      filter(as.character(!!quo_x) %in% data_labels_at)
  }
  
  
  # CREATE LINE LAYER -------------------------------------------------------
  
  if (quo_is_null(quo_color)) {
    geom <- geom_line(aes(x = !!quo_x,
                          y = !!quo_y,
                          group = 1),
                      color = color,
                      data = data,
                      linewidth = 1.5)
  } else {
    geom <- geom_line(aes(x = !!quo_x,
                          y = !!quo_y,
                          group = !!quo_color,
                          color = !!quo_color),
                      data = data,
                      linewidth = 1.5)
  }
  
  # CREATE POINT LAYER -------------------------------------------------------
  
  #write warnings for functionalities that are easy to use incorrectly
    if (!is.null(points_at) && points != "specific") {
    warning("`points` does not equal `specific`, so `points_at` is ignored")
  }
  
  if (points == FALSE | points == "none") {
    point <- NULL
  } else if (quo_is_null(quo_color)) {
    point <- geom_point(aes(x = !!quo_x,
                            y = !!quo_y),
                        data = points_data,
                        shape = 21,
                        size = 1.5,
                        stroke = 1,
                        fill = "white", 
                        color = color)
  } else {
    point <- geom_point(aes(x = !!quo_x,
                            y = !!quo_y,
                            color = !!quo_color),
                        data = points_data,
                        shape = 21,
                        size = 1.5,
                        stroke = 1,
                        fill = "white")
  }
  
  
  # CREATE TEXT LAYER -------------------------------------------------------
  
  if (data_labels_color == TRUE && quo_is_null(quo_color)) {
    warning("`color_by` = NULL, so `data_labels_color` is ignored")
  }
  
  if (data_labels == FALSE | data_labels == "none") {
    labels <- NULL
  } else if (data_labels_color == FALSE) {
    labels <- geom_text(aes(x = !!quo_x,
                            y = !!quo_y,
                            label = !!quo_y,
                            hjust = label_hjust,
                            vjust = label_vjust),
                        data = labels_data,
                        fontface = "___", #your style
                        family = "___", #your font
                        size = 3)
  } else {
    labels <- geom_text(aes(x = !!quo_x,
                            y = !!quo_y,
                            color = !!quo_color,
                            label = !!quo_y,
                            hjust = label_hjust,
                            vjust = label_vjust),
                        data = labels_data,
                        fontface = "___", #your style
                        family = "___", #your font
                        size = 3)
    
  }
  
  # CREATE FACET LAYER ------------------------------------------------------
  
  if (quo_is_null(quo_facet) && facet_ncol != 1) {
    warning("`facet_by` = NULL, so `facet_ncol` is ignored")
  } 
  
  if (quo_is_null(quo_facet)) {
    facet <- NULL
  } else {
    facet <- facet_wrap(quo_facet,
                        scales = facet_scales,
                        ncol = facet_ncol)
  }
  
  # CREATE AXIS LAYER -------------------------------------------------------
  #delete if you don't want a capped axis
  axis_style <- coord_capped_cart(bottom = capped_horizontal())
  
  # GENERATE PLOT -----------------------------------------------------------
  #if you add or take out any objects, add or remove from list function
  #e.g. if axis not capped, take out axis_style
  list(geom, point, labels, facet, axis_style)
  
}
