source("scripts.R")

boxes = data_nest$subject

df = data_nest

server <- function(input, output) {
  
  global_vars = reactiveValues()
  
  source("sidebar_items_server.R", local = TRUE)
  
  source("individual_plot_server.R", local = TRUE)
  
  source("grouped_plot_server.R", local = TRUE)
  
  source("daily_individual_plot_server.R", local = TRUE)
  
  source("daily_grouped_plot_server.R", local = TRUE)
  
  source("hour_plot.R", local = TRUE)
  
  source("download.R", local = TRUE)
  
}