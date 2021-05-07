# imports ------------------------------------------------
library(shiny)
library(shinyWidgets)
library(data.table)
library(xlsx)
library(rJava)
library(stringr)
library(readr)
library(tidyverse)
library(purrr)
library(broom)
library(ggplot2)
library(plotly)
library(scales)
library(numbers)
library(reshape2)
library(matrixStats)
library(lubridate)
library(dplyr)

# options ------------------------------------------------
options(shiny.maxRequestSize = 20*1024^2)
options(java.parameters = "-Xmx2048m")

# functions -----------------------------------------------
find_interval = function(df, group_col, date_time_col, id_col) {
  date_time_df = df %>% pivot_wider(names_from = {{group_col}}, values_from = {{date_time_col}}, id_cols = {{id_col}})
  interval = map_dfr(date_time_df[-1], function(x) {diff(x %>% as_datetime) %>% as.integer}) #%>%
  #pivot_longer(everything(), names_to = "subject", values_to = "interval") %>%
  #dplyr::select(interval) #%>%
  # unique %>%
  # pull
  return(interval)
}

create_aggregation_vector = function(each, length) {
  vec = c(rep(1:length, each = each, length.out = length))
  return(vec)
}

aggregate_parameter = function(data, time, param) {
  func = aggregate_by(param)
  data = setDT(data)[,.(light = data.table::first(light),
                 date_time = data.table::first(date_time),
                 mean = func(get(param)),
                 param = param),
              by = .(subject, interval = get(time))]
  data[,period := cumsum(c(1,diff(light)!=0)), by = subject]
  return(data)
}

parse_group_inputs = function(inp) {
  no_groups = 1
  if(!is.null(inp$select_no_groups)) {
    no_groups = inp$select_no_groups
  }
  
  group_list = lapply(1:as.integer(no_groups), function(x) {
    inp[[paste0("group_no_", x)]]
  })
  
  group_list[sapply(group_list, is.null)] <- list("")
  group_list <- group_list[group_list != ""]
  
  group_list <- rapply(lapply(group_list, strsplit, ","), str_trim, how = "list") %>%
    lapply(unlist)
  
  if(length(group_list) > 0) {
    group_df = map_dfr(1:length(group_list), function(x) {
      cbind.data.frame(subject = group_list[[x]], group =  paste0("Group", x), stringsAsFactors = FALSE)
    })
  } else {
    group_df = data.frame()
  }
  
  return(group_df)
}

# min.mean.sd.max <- function(x) {
#   r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
#   names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
#   r
# }
# 
# mean.sd <- function(x) {
#   r <- c(mean(x) - sd(x), mean(x), mean(x) + sd(x))
#   names(r) <- c("ymin", "y", "ymax")
#   r
# }


aggregate_with_specs = function(specs) {
  function(select_param) {
    by = specs %>% dplyr::filter(name_app == select_param) %>% dplyr::select(aggregate) %>% as.character
    return(get(by))
  }
}


# additional ploting options ---------------------------------
# TODO add geom_tile

plot_points = function(condition_field, aes_colour = subject) {
  if("1" %in% condition_field) {
    geom_point(aes(colour = {{ aes_colour }} ), size = 0.75)
  } else {
    geom_blank()
  }
}

plot_errorbars = function(condition_field, aes_fill = group) {
  if(condition_field == "2") {
    geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd, fill = {{ aes_fill }}), alpha = 0.08)
  } else {
    geom_blank()
  }
}

plot_facets = function(n, formula = "param ~ ." ) {
  if(n > 1) {
    facet_grid(as.formula(formula) ,scales = "free_y", labeller = label_both)
  } else {
    geom_blank()
  }
}

# plot_jitter = function(condition_field) {
#   if("1" %in% condition_field) {
#     geom_jitter(shape = 21, colour = "grey50", fill = "white", size = 1, stroke = 0.25, width = 0.25)
#   } else {
#     geom_blank()
#   }
# }

# constants ------------------------------------------------
column_specs = suppressMessages(read_delim("clams_column_specification.txt", delim = '\t'))
aggregate_by = aggregate_with_specs(column_specs)

# server ---------------------------------------------------
server <- function(input, output, session) {
  
  theme_set(theme_minimal(base_size = 12))
  
  rv_data = reactiveValues(
    interval = NULL,
    data_agg = data.frame(),
    current_view = NULL, 
    column_specs = data.frame(),
    parameters = NULL,
    subject_list = NULL,
    time_aggregation_values = NULL,
    time_aggregation_repeats = NULL
  )
  
  rv_filters = reactiveValues(
    aggregation = 60,
    counter = 1,
    parameters = "vo2",
    subjects = NULL,
    max_interval = 0,
    groups = NULL,
    hour = FALSE,
    dark_periods = NULL,
    light_periods = NULL,
    scatter_x = "vo2",
    scatter_y = "vco2",
    scatter_size = "heat"
  )
  
  rv_options = reactiveValues(
    plot_width = 1500,
    plot_height = 500,
    height_multiplier = 1
  )
  
  observe({
    print("plot_options_running")
    rv_options$plot_width = input$plot_width
    rv_options$plot_height = input$plot_height
    rv_options$height_multiplier = length(rv_filters$parameters)
  })
  
  source("read_input.R", local = TRUE)
  source("reactive_exprs.R", local = TRUE)
  source("sidebar_items.R", local = TRUE)
  source("individual_series_plot.R", local = TRUE)
  source("individual_summary_plot.R", local = TRUE)
  source("individual_scatter_plot.R", local = TRUE)
  source("individual_activity_plot.R", local = TRUE)
  source("individual_hour_plot.R", local = TRUE)
  source("grouped_series_plot.R", local = TRUE)
  source("grouped_summary_plot.R", local = TRUE)
  source("grouped_scatter_plot.R", local = TRUE)
  source("grouped_activity_plot.R", local = TRUE)
  source("grouped_hour_plot.R", local = TRUE)
  source("download.R", local = TRUE)
}
