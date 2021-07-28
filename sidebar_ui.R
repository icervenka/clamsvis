output$individual_grouped_select = renderUI({
  radioButtons("individual_grouped_select", "Select display mode",
    choices = c("grouped" = "group", "individual" = "subject"),
    selected = "group"
  )
})

output$select_subjects = renderUI({
  req(input$individual_grouped_select == "subject")

  pickerInput(
    inputId = "select_subjects",
    label = "Select subjects",
    choices = get_subjects(),
    selected = get_subjects()[1],
    options = list(
      `actions-box` = TRUE,
      size = 15,
      `selected-text-format` = "count > 4"
    ),
    multiple = TRUE
  )
})

param_select_inputs = eventReactive(
  {
    rv_counters$param_select
    input$load_file
  },
  {
    n = rv_counters$param_select
    param_select = lapply(seq_len(n), function(i) {
      selectInput(paste0("select_parameter_", i),
        label = NULL,
        choices = get_parameters(),
        selected = rv$selected_params[i]
      )
    })
    return(param_select)
  }
)

output$parameter_select = renderUI({
  list(
    tags$strong(tags$p("Select parameter")),
    param_select_inputs(),
    fluidRow(
      column(
        12,
        actionButton("add_param_select_btn", "Add", width = "48%", class = "btn-success"),
        actionButton("rm_param_select_btn", "Remove", width = "48%", class = "btn-danger")
      )
    ),
    tags$hr()
  )
})

output$select_aggregation = renderUI({
  shinyWidgets::sliderTextInput("select_aggregation",
    "Select aggregation [min]",
    choices = valid_freq(),
    selected = rv$selected_aggregation
  )
})

output$display_interval = renderUI({
  sliderInput("display_interval",
    label = "Display intervals",
    min = 1,
    max = get_max_interval(),
    value = c(rv$selected_interval_low, rv$selected_interval_high),
    step = 1
  )
})

output$select_phase = renderUI({
  list(
    pickerInput(
      inputId = "select_dark",
      label = "Include dark periods",
      choices = get_dark_periods(),
      selected = get_dark_periods(),
      options = list(
        `actions-box` = TRUE,
        size = 15,
        `selected-text-format` = "count > 15"
      ),
      multiple = TRUE
    ),
    pickerInput(
      inputId = "select_light",
      label = "Include light periods",
      choices = get_light_periods(),
      selected = get_light_periods(),
      options = list(
        `actions-box` = TRUE,
        size = 15,
        `selected-text-format` = "count > 15"
      ),
      multiple = TRUE
    )
  )
})

output$download_view = renderUI({
  downloadButton("download_current_view", label = "Download View", icon = icon("download"))
})

output$download_current_view = downloadHandler(
  filename = "current_view.csv",
  content = function(file) {
    data = rv$current_view %>%
      mutate(across(where(is.numeric), ~ round(., digits = 5)))
    write.csv(data, file, row.names = F, quote = F)
  }
)

output$select_cumulative = renderUI({
  radioButtons("select_cumulative",
               label = "Plot",
               choices = list("Interval data" = "mean", "Cumulative data" = "cumsum"),
               selected = "mean"
  )
})

output$display_point_markers = renderUI({
  list(
    tags$hr(),
    checkboxGroupInput("display_point_markers", NULL,
      choices = c("Show point markers" = 1)
    )
  )
})

output$display_stat_header = renderUI({
  req(input$individual_grouped_select == "group")
  list(
    tags$hr(),
    tags$div(tags$p(tags$b("Display statistics")))
  )
})

output$display_sd = renderUI({
  req(input$individual_grouped_select == "group")
  checkboxGroupInput("display_sd", NULL,
    choices = c("Show SD" = 1)
  )
})

output$display_stat_table = renderUI({
  req(input$individual_grouped_select == "group")
  checkboxGroupInput("display_stat_table", NULL,
    choices = c("Show stat table" = 1)
  )
})

output$shift_zt = renderUI({
  sliderInput("shift_zt",
    label = "Shift Zeitgeber 0 to:", min = 0,
    max = 23, value = 6, step = 1
  )
})

output$scatter_controls = renderUI({
  list(
    pickerInput(
      inputId = "scatter_x",
      label = "X axis",
      choices = get_parameters()
    ),
    pickerInput(
      inputId = "scatter_y",
      label = "Y axis",
      choices = get_parameters()
    ),
    pickerInput(
      inputId = "scatter_size",
      label = "Size",
      choices = c("NULL", get_parameters())
    ),
    checkboxGroupInput("plot_trendline", NULL,
                       choices = c("Plot trendline" = 1)
    )
  )
})

output$activity_controls = renderUI({
  list(
    selectInput("select_activity_parameter",
                label = NULL,
                choices = get_parameters(),
                selected = rv$selected_params[1]
    ),
    shinyWidgets::sliderTextInput("activity_aggregation",
      "Activity window [min]",
      choices = valid_freq(),
      selected = "60"
    ),
    numericInput("activity_mincount", label = "Min value per activity window", value = 5)
  )
})

output$plot_width = renderUI({
  sliderInput("plot_width",
    label = "Plot width [px]", min = 1000,
    max = 2000, value = 1500
  )
})

output$plot_height = renderUI({
  sliderInput("plot_height",
    label = "Plot height [px]", min = 250,
    max = 1000, value = 500
  )
})
