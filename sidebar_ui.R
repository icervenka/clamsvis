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
