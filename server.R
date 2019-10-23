# imports ------------------------------------------------
library(shiny)
library(shinyWidgets)
library(xlsx)
library(rJava)
library(stringr)
library(readr)
library(tidyverse)
library(purrr)
library(broom)
library(ggplot2)
library(numbers)
library(reshape2)
library(matrixStats)
library(lubridate)
library(dplyr)

# options ------------------------------------------------
options(shiny.maxRequestSize = 20*1024^2)
options(java.parameters = "-Xmx2048m")

# functions -----------------------------------------------
create_aggregation_vector = function(each, length) {
  vec = c(rep(1:length, each = each, length.out = length))
  return(vec)
}

aggregate_parameter = function(data, subject, aggregation, parameter = "vo2", time = "t360", by = "mean", cumulative = FALSE) {
  param = parameter
  tt = aggregate(data %>% dplyr::select(date_time, light), aggregation %>% dplyr::select(time), first)
  dt = aggregate(data %>% dplyr::select(parameter), aggregation %>% dplyr::select(time), by)
  names(dt) = c("interval", "parameter")
  if(cumulative == TRUE) {
    dt = cbind.data.frame(dt %>% dplyr::select(-parameter), dt %>% dplyr::select(parameter) %>% cumsum)
  }
  return(cbind.data.frame(tt, dt, subject, param, stringsAsFactors = F))
}

parse_group_inputs = function(inp) {
  
  group_list = lapply(1:as.integer(inp$select_no_groups), function(x) {
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

aggregate_by = function(select_param) {
  by = column_specs %>% dplyr::filter(name_app == select_param) %>% dplyr::select(aggregate) %>% as.character
  return(by)
}

min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

aggregate_with_specs = function(specs) {
  function(select_param) {
    by = specs %>% dplyr::filter(name_app == select_param) %>% dplyr::select(aggregate) %>% as.character
    return(by)
  }
}

# constants ------------------------------------------------
column_specs = read_delim("clams_column_specification.txt", delim = '\t')
aggregate_by = aggregate_with_specs(column_specs)
interval = 2

# server ------------------------------------------------
server <- function(input, output) {
  
  theme_set(theme_bw(base_size = 18))
  global_vars = reactiveValues()
  
  counter = reactiveValues(n = 1)
  
  observeEvent(input$add_btn, {counter$n <- counter$n + 1})
  observeEvent(input$rm_btn, {
    if (counter$n > 1) counter$n <- counter$n - 1
  })
  
  paramter_boxes <- reactive({
  
    n <- counter$n
    
    if (n > 1) {
      lapply(seq_len(n-1), function(i) {
        selectInput(paste0("select_parameter_",i), label = NULL, 
                    choices = global_vars$parameters, 
                    selected = input[[paste0("select_parameter_", i)]])
      })
    }
    
  })
  
  output$textbox_ui <- renderUI({ paramter_boxes() })
  
  source("sidebar_items_server.R", local = TRUE)
  
  source("read_input_server.R", local = TRUE)
  
  source("individual_plot_server.R", local = TRUE)
  
  source("grouped_plot_server.R", local = TRUE)
  
  source("daily_individual_plot_server.R", local = TRUE)
  
  source("daily_grouped_plot_server.R", local = TRUE)
  
  source("hour_plot_server.R", local = TRUE)
  
  source("download_server.R", local = TRUE)
  
}
