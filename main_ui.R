# parameter assignment ------
# from input form
custom_param_inputs = reactive({
  n = rv_counters$param_input
  required = list(
    fluidRow(
      column(
        3,
        selectInput("subject",
                    label = "Subject column name",
                    choices = custom_colnames(),
                    selected = input[["subject"]]
        )
      )
    ),
    fluidRow(
      column(
        3,
        selectInput("date_time",
                    label = "Date-Time column name",
                    choices = custom_colnames(),
                    selected = input[["date_time"]]
        )
      )
    ),
    fluidRow(
      column(
        3,
        selectInput("light",
                    label = "Light column name",
                    choices = c("infer", custom_colnames()),
                    selected = NULL
        )
      ),
      column(
        3,
        timeInput("light_start",
                  "Start of light phase",
                  value = rv$ils
        )
      ),
      column(
        3,
        timeInput("light_end",
                  "End of light phase",
                  value = rv$ile
        )
      )
    )
  )

  header_optional = fluidRow(
    hr(),
    column(3, h5("Parameter")),
    column(3, h5("Name to display")),
    column(3, h5("Aggregate by")),
    column(3, h5("Bout"))
  )

  optional = lapply(seq_len(n), function(i) {
    fluidRow(
      column(
        3,
        selectInput(paste0("parameter_id_", i),
                    label = NULL,
                    choices = custom_colnames(),
                    selected = input[[paste0("parameter_id_", i)]]
        )
      ),
      column(
        3,
        textInput(paste0("parameter_name_", i),
                  label = NULL,
                  value = input[[paste0("parameter_name_", i)]]
        )
      ),
      column(
        3,
        selectInput(paste0("parameter_agg_", i),
                    label = NULL,
                    choices = list("mean", "sum", "first"),
                    selected = input[[paste0("parameter_agg_", i)]]
        )
      ),
      column(
        3,
        prettyCheckbox(paste0("parameter_bout_", i),
                       label = NULL,
                       value = input[[paste0("parameter_bout_", i)]],
                       bigger = T
        )
      )
    )
  })
  return(list(required, header_optional, optional))
})

output$custom_file_specs = renderUI({
  req(file_type() == "custom")

  list(
    tags$div(
      tags$p("File format not recognized, please provide the column specification below.
               In case the room light has to be inferred from Date-Time,
               the start and end of light phase has to be specified.",
        class = "text-muted"
      )
    ),
    custom_param_inputs(),
    fixedRow(
      column(
        3,
        actionButton("add_param_input_btn",
          "Add",
          class = "btn-success",
          width = "100%"
        ),
      ),
      column(
        3,
        actionButton("rm_param_input_btn",
          "Remove",
          class = "btn-danger",
          width = "100%"
        ),
      )
    ),
    br()
  )
})

assign_group_inputs = eventReactive(
  {
    input$add_group_btn
    input$rm_group_btn
    get_subjects()
  },
  {
    n = rv_counters$group

    boxes = lapply(seq_len(n), function(i) {
      fluidRow(
        column(
          3,
          pickerInput(
            inputId = paste0("group_no_", i),
            label = paste0("Group ", i),
            choices = get_subjects(),
            selected = input[[paste0("group_no_", i)]],
            options = list(
              `actions-box` = TRUE,
              size = 15,
              `selected-text-format` = "count > 4"
            ),
            multiple = TRUE
          )
        ),
        column(
          3,
          textInput(paste0("group_name_", i),
                    label = "Group Name",
                    value = input[[paste0("group_name_", i)]]
          )
        )
      )
    })
    return(list(boxes))
  }
)

# group assignment ------
# from input form

output$load_groups_file = renderUI({
  list(
    br(),
    fluidRow(
      column(
        3,
        fileInput("metadata", label = NULL)
      )
    )
  )
})

output$assign_groups_file = renderUI({
  req(!is.null(input$metadata))
  fluidRow(
    column(
      3,
      selectInput("metadata_group_col",
                  label = "Select group column",
                  choices = get_metadata_header()

      )
    )
  )
})

output$render_group_info = renderUI({
  req(rv$file_upload_valid == 1)
  list(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "File",
        list(
          uiOutput('load_groups_file'),
          uiOutput('assign_groups_file'),
          fluidRow(
            column(
              3,
              actionButton("update_groups_file",
                           "Update groups",
                           width = "100%",
                           class = "btn-primary"
              )
            )
          )
        )
      ),
      tabPanel(
        "Input",
        list(
          br(),
          assign_group_inputs(),
          fluidRow(
            column(
              3,
              actionButton("add_group_btn",
                "Add",
                class = "btn-success",
                width = "100%"
              )
            ),
            column(
              3,
              actionButton("rm_group_btn",
                "Remove",
                class = "btn-danger",
                width = "100%"
              )
            )
          ),
          br(),
          fluidRow(
            column(
              3,
              actionButton("update_groups_input",
                           "Update groups",
                           width = "100%",
                           class = "btn-primary"
              ))
          )
        )
      )
    ),
    br(),
    fluidRow(
      column(
        3,
        actionButton("preview_data",
          "Preview",
          width = "100%",
          class = "btn-primary"
        )
      )
    )
  )
})

# data preview ------
output$preview = renderUI({
  shinyjs::hidden(
    div(id = "prev_div",
      hr(),
      h4("Data Preview"),
      renderTable(head(read_data()), striped = T)
    )
  )
})

# download preview ------
output$download = renderUI({
  default_aggregations = c(10, 30, 60, 180, 720)

  fluidRow(
    column(
      12,
      tags$br(),
      purrr::map(1:5, function(x) {
        selectInput(paste0("select_aggregation_", x),
          paste0(label = "Select aggregation ", x, " [min]"),
          choices = valid_freq(),
          selected = default_aggregations[x]
        )
      }),
      downloadButton("download_xlsx",
        label = "Download",
        icon = icon("download")
      )
    )
  )
})

output$download_xlsx = downloadHandler(
  filename = function() {
    paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_clams_vis", ".xlsx")
  },
  content = function(file) {
    tagg = purrr::map(1:5, function(x) {
      input[[paste0("select_aggregation_", x)]]
    }) %>% as.vector(mode = "integer")

    data_long_agg = purrr::map_dfr(tagg, function(x) {
      df = aggregate_selected_params(get_data_agg_combined(), x, get_parameters())
      cbind.data.frame(df, aggregation = x, stringsAsFactors = FALSE)
    })

    wb = xlsx::createWorkbook(type = "xlsx")

    purrr::walk(get_parameters(), function(p, datap) {
      sheet = xlsx::createSheet(wb, sheetName = p)
      dfp = datap %>% filter(param == p)

      purrr::walk(seq_along(tagg), function(a, dataa) {
        dfa = dataa %>%
          dplyr::filter(aggregation == tagg[a]) %>%
          tidyr::pivot_wider(
            id_cols = c(interval, date_time, light),
            names_from = subject,
            values_from = mean
          )

        xlsx::addDataFrame(as.data.frame(dfa),
          sheet,
          startRow = 1,
          startColumn = (dim(dfa)[2] + 1) * (a - 1) + 1,
          row.names = FALSE
        )
      }, dataa = dfp)
    }, datap = data_long_agg)
    xlsx::saveWorkbook(wb, file)
  }
)
