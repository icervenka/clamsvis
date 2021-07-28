# imports ------------------------------------------------
library(magrittr)
library(shiny)
library(shinyWidgets)
library(shinyTime)
library(reactlog)
library(plotly)
source("functions.R", local = TRUE)

# options ------------------------------------------------
options(shiny.maxRequestSize = 20*1024^2)
options(java.parameters = "-Xmx2048m")

# constants ------------------------------------------------
column_specs = suppressMessages(read_delim("clams_column_specification.txt", delim = '\t'))
aggregate_by = aggregate_with_specs(column_specs)

### constants ------------------------------------------------------------------
parse_patterns = list(
  "clams" = list(
    file_pattern = "Oxymax CSV File|:DATA",
    header_pattern = "INTERVAL,CHAN,DATE/TIME",
    data_start_offset = 3,
    data_end_pattern = ":EVENTS"
  ),
  "tse" = list(
    file_pattern = "Date,Time,Animal",
    header_pattern = "Date,Time,Animal",
    data_start_offset = 2,
    data_end_pattern = NULL
  ) # ,
  # "tsconvert" = list(
  #   file_pattern = "subject,interval,date_time", # "[TS convert Metabolomics datafile]
  #   header_pattern = "subject,date_time,light",
  #   data_start_offset = 1,
  #   data_end_pattern = NULL
  # ),
  # "tsconvert_fwr" = list(
  #   file_pattern = "subject,interval,date_time", # "[TS convert FWR datafile]
  #   header_pattern = "subject,date_time,light",
  #   data_start_offset = 1,
  #   data_end_pattern = NULL
  # ),
)

# server ---------------------------------------------------
server <- function(input, output, session) {

  rv = reactiveValues(
    file_upload_valid = 0,
    update_groups_input_toggle = 0,
    update_groups_file_toggle = 0,
    global_frequency = NULL,
    selected_params = NULL,
    selected_subjects = NULL,
    selected_aggregation = NULL,
    selected_interval_low = NULL,
    selected_interval_high = NULL,
    selected_periods_dark = NULL,
    selected_periods_light = NULL,
    current_view = NULL
  )

  rv_counters = reactiveValues(
    param_input = 1,
    param_select = 1,
    group = 2
  )

  source("sidebar_ui.R", local = TRUE)
  source("main_ui.R", local = TRUE)
  source("read_input.R", local = TRUE)
  source("reactive_exprs.R", local = TRUE)
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
