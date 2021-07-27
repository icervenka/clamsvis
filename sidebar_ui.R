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
