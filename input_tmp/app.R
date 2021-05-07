library(dplyr)
library(purrr)
library(shiny)
library(tidyr)
library(lubridate)
library(shinyWidgets)
library(shinyTime)
library(reactlog)

# maybe offload to json so it can be easier to modify
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
  )#,
  # "ts_convert" = list(
  #   file_pattern = "subject,interval,date_time", # "[TS convert datafile]
  #   header_pattern = "subject,interval,date_time",
  #   data_start_offset = 1,
  #   data_end_pattern = NULL
  # )
)

infer_file_type = function(file, parse_patterns) {
  file_patterns = map(names(parse_patterns), function(x) {
    parse_patterns[[x]][['file_pattern']]
  }) %>% setNames(names(parse_patterns))
  
  found_pattern = map(names(file_patterns), function(x, file_lines) {
    grepl(file_patterns[[x]], file_lines) %>% sum()
  }, file_lines = readLines(file)) %>% 
    setNames(names(file_patterns))
  
  found_pattern_bool = found_pattern > 0
  
  # TODO I need to catch file type not recognized to display custom form later
  if(sum(found_pattern_bool) == 1) {
    return(which(found_pattern_bool) %>% names)
  } else if(sum(found_pattern_bool) == 0) {
    print("File type not recognized")
    return("custom")
  } else if(sum(found_pattern_bool) > 1) {
    print("More than one type fits the criteria")
    return(NULL)
  } else {
    print("Unknown error.")
    return(NULL)
  }
}

validate_param_df = function(parameter_df) {
  if(anyDuplicated(parameter_df$select) > 0) {
    print("Duplicated parameters selected.")
    return(FALSE)
  } else if(anyDuplicated(parameter_df$select) > 0) {
    print("Duplicated parameter display names.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

remove_empty_lines = function(file_lines) {
  file_lines[nchar(file_lines) != 0]
}

parse_file = function(filepath, parse_pattern) {
  file_lines = remove_empty_lines(readLines(filepath))
  
  header_line = grep(parse_pattern$header_pattern, file_lines)[1]
  data_start_line = header_line + parse_pattern$data_start_offset
  data_end_line = ifelse(is.null(parse_pattern$data_end_pattern), 
                    length(file_lines), 
                    grep(parse_pattern$data_end_pattern, file_lines)[1] - 1)
  
  header = file_lines[header_line] %>% str_split("\\s*,\\s*", simplify = T)
  df = map_dfr(file_lines[data_start_line:data_end_line], function(x) {
    str_split(x, "\\s*,\\s*", simplify = T) %>%
      as.data.frame(stringsAsFactors = F)
  }) %>% setNames(header)
  return(df)
}

prettify_data = function(df, col_specs, type) {
  
  df = df %>%
    select(col_specs$colname[!is.na(col_specs$colname)]) %>%
    setNames(col_specs %>% select(colname, app) %>% drop_na %>% pull(app))
  
  df = map_dfc(df, ~ type.convert(., as.is = TRUE))
  
  if(type == "clams") {
    df = df %>%
      mutate(xyt = xt + yt,
             xf = xt - xa,
             yf = yt - ya) %>%
      mutate(light = case_when(light == "ON" ~ 1,
                               light == "OFF" ~ 0,
                               TRUE ~ 0))
  }

  if(type == "tse") {
    # TODO maybe set light level as a parameter from user
    df = df %>%
      mutate(date_time = paste0(Date, " ", Time)) %>%
      mutate(light = case_when(light > 30 ~ 1,
                               TRUE ~ 0))
  }
  
  df$date_time = infer_date_time(df$date_time)
  df = df %>%
    select(col_specs$app[!is.na(col_specs$app)])
  
  return(as.data.frame(df))
}

infer_date_time = function(column) {
  parse_date_time(column, orders = c("mdY T", "dmY T", "Ymd T", "Ydm T"))
}

infer_light = function(df, start, end) {
  start = hms(start)
  end = hms(end)
  df$date_time = infer_date_time(df$date_time)
  df = df %>% 
    mutate(light = ifelse(hms::as_hms(date_time) >= start & hms::as_hms(date_time) < end, 1, 0))
  return(df)
}

### ui -------------------------------------------------------------------------
ui = fluidPage(
  fluidRow(
    column(3,
           fileInput("file", label = NULL))
  ),
  uiOutput('custom_file_specs'),
  fluidRow(
    column(3,
           actionButton("load_file", 
                        "Load File", 
                        width = '100%', 
                        class = "btn-primary")
    )
  ),
  br(),
  uiOutput('render_group_info'),
  fluidRow(column(12, uiOutput("preview")))
)

### server ---------------------------------------------------------------------
server = function(input, output) {
  
  rv = reactiveValues(
    file_upload_valid = 0,
    custom_subject_colname = NULL,
    custom_light_colname = NULL,
    ils = strptime("06:00:00", "%T"),
    ile = strptime("18:00:00", "%T")
  )
  
  rv_counters = reactiveValues(
    param = 1,
    group = 2
  )
  
  observeEvent(input$file, {
    rv$file_upload_valid = 0
  })
  
  observeEvent(input$load_file, {
    rv$file_upload_valid = 1
  })
  
  observeEvent({
    input$light_start
    input$light_end},
    {
      rv$ils = input$light_start
      rv$ile = input$light_end
    })
  
  file_type = eventReactive(input$file, {
    infer_file_type(input$file$datapath, parse_patterns)
  }, ignoreNULL = T)
  
  
  custom_colnames = reactive({
    if(file_type() == "custom") {
      names(read.csv(input$file$datapath))
    }
  })
  
  observeEvent(input$add_param_btn, {
    if(rv_counters$param < 20) rv_counters$param <- rv_counters$param + 1
  })
  
  observeEvent(input$rm_param_btn, {
    if(rv_counters$param > 1) rv_counters$param <- rv_counters$param - 1
  })
  
  custom_param_inputs = reactive({
    n <- rv_counters$param
    isolate({
    required = list(
      fluidRow(
        column(3,
               selectInput("subject", label = "Subject column name", 
                           choices = custom_colnames(), 
                           selected = input[['subject']]))
      ),
      fluidRow(
        column(3,
               selectInput("date_time", label = "Date-Time column name", 
                           choices = custom_colnames(), 
                           selected = input[['date_time']]))
      ),
      fluidRow(
        column(3,
               selectInput("light", label = "Light column name", 
                           choices = c("infer", custom_colnames()), 
                           selected = NULL)),
        column(3,
               timeInput("light_start", 
                         "Start of light phase", 
                         value = rv$ils)),
        column(3,
               timeInput("light_end", 
                         "End of light phase", 
                         value = rv$ile))
      )
    )
    
    header_optional = fluidRow(
      hr(),
      column(3, h5("Parameter")),
      column(3, h5("Name to display")),
      column(3, h5("Aggregate by")),
      column(3, h5("Bout"))
    )
    #if (n >= 1) {
    optional = lapply(seq_len(n), function(i) {
      fluidRow(
        column(3,
               selectInput(paste0("parameter_id_",i), 
                           label = NULL,
                           choices = custom_colnames(),
                           selected =  input[[paste0("parameter_id_", i)]])),
        column(3,
               textInput(paste0("parameter_name_",i), 
                         label = NULL, 
                         value = input[[paste0("parameter_name_", i)]])),
        column(3,
               selectInput(paste0("parameter_agg_",i), 
                           label = NULL,
                           choices = list("mean", "sum", "first"),
                           selected = input[[paste0("parameter_agg_", i)]])),
        column(3,
               prettyCheckbox(paste0("parameter_bout_",i), 
                              label = NULL, 
                              value = input[[paste0("parameter_bout_",i)]], 
                              bigger = T))
      )
    })
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
               class="text-muted")
      ),
      custom_param_inputs(),
      fixedRow(
        column(3,
               actionButton("add_param_btn", 
                            "Add", 
                            class = "btn-success",
                            width = '100%'),
        ),
        column(3,
               actionButton("rm_param_btn", 
                            "Remove", 
                            class = "btn-danger",
                            width = '100%'),
        )
      ),
      br()
    )
  })
  
  observeEvent(input$add_group_btn, {
    if(rv_counters$group < 12) rv_counters$group <- rv_counters$group + 1
  })
  
  observeEvent(input$rm_group_btn, {
    if(rv_counters$group > 2) rv_counters$group <- rv_counters$group - 1
  })
  
  output$render_group_info = renderUI({
    req(rv$file_upload_valid == 1)
    list(
      tabsetPanel(type = "tabs",
                  tabPanel("Input",
                    list(
                      br(),
                      assign_group_inputs(),
                      fluidRow(
                        column(3,
                               actionButton("add_group_btn", 
                                            "Add",
                                            class = "btn-success",
                                            width = '100%')),
                        column(3,
                               actionButton("rm_group_btn", 
                                            "Remove", 
                                            class = "btn-danger",
                                            width = '100%'))
                      ),
                      br()
                    )),
                  tabPanel("File",
                           list(
                             load_groups_file(),
                             assign_groups_file()
                           ))
      ),
      fluidRow(
        column(3,
               actionButton("update_groups", 
                            "Update groups", 
                            width = '100%', 
                            class = "btn-primary")
        ),
        column(3,
               actionButton("preview_data", 
                            "Preview", 
                            width = '100%', 
                            class = "btn-primary")
        )
      )
    )
  })
  
  
  # ,
  # tabPanel("File",
  #          list(
  #            load_groups_file(),
  #            assign_groups_file()
  #          ))
  # ,
  # tabPanel("File", output$load_groups_file,
  #          output$assign_groups_file),
  
  assign_group_inputs = eventReactive(
    {
      input$add_group_btn
      input$rm_group_btn
      get_subjects()
    }, {
      n = rv_counters$group
      
      boxes = lapply(seq_len(n), function(i) {
        fluidRow(
          column(3,
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
         column(3,
                textInput(paste0("group_name_",i), 
                          label = "Group Name", 
                          value = input[[paste0("group_name_", i)]])
                )
          )
        })
    return(list(boxes))
  })
  
  output$assign_groups = renderUI({ 
    list(
      br(),
      assign_group_inputs(),
      fixedRow(
        column(3,
               actionButton("add_group_btn", 
                            "Add", 
                            width = '100%')),
        column(3,
               actionButton("rm_group_btn", 
                            "Remove", 
                            width = '100%'))
      ),
      br()
    )
  })
  
  create_param_df = eventReactive(input$load_file, {
    if(file_type() == "custom") {
      
      required = data.frame(colname = c(input[['subject']], 
                                        input[['date_time']], 
                                        ifelse(input[['light']] == "infer", "light", input[['light']])),
                            display = c("Subject", "Date-Time", "Light"),
                            app = c("subject", "date_time", "light"),
                            required = 1,
                            aggregate = c(NA, "first", "first"),
                            bout = c(FALSE, FALSE, FALSE),
                            stringsAsFactors = F)
      
      optional = map_dfr(1:rv_counters$param, function(x) {
        data.frame(colname = input[[paste0("parameter_id_", x)]],
                   display= ifelse(nchar(input[[paste0("parameter_name_", x)]]) == 0,
                                 input[[paste0("parameter_id_", x)]],
                                 input[[paste0("parameter_name_", x)]]),
                   app = input[[paste0("parameter_id_", x)]],
                   required = 0,
                   aggregate = input[[paste0("parameter_agg_", x)]],
                   bout = input[[paste0("parameter_bout_", x)]],
                   stringsAsFactors = F)
      })
      
      param_df = rbind.data.frame(required,optional)
    } else if(!is.null(file_type())) {
      param_df = read.table("clams_colspecs.txt", sep = "\t", header = T)
    }
    return(param_df)
  })
  
  # observeEvent(input$light, {
  #   insUI = tags$div(id = "phase_select",
  #     fluidRow(
  #     column(3,
  #            timeInput("light_start", 
  #                      "Start of light phase", 
  #                      value = strptime(input$light_start, "%T"))),
  #     column(3,
  #            timeInput("light_end", 
  #                      "End of light phase", 
  #                      value = strptime("18:00:00", "%T"))))
  #   )
  #   
  #   if(input$light == "infer") {
  #     insertUI(
  #       selector="#light_select",
  #       where="beforeEnd",
  #       ui={ insUI })
  #   } else {
  #     removeUI(selector = "#phase_select")
  #   }
  # })
  
  observe({
    create_param_df() %>% validate_param_df()
  })
  
  load_groups_file = reactive({
    list(
      br(),
      fluidRow(
        column(3,
               fileInput("metadata", label = NULL))
      )
    )
  })
  
  assign_groups_file = reactive({
    fluidRow(
      column(3,
             selectInput("metadata_group_col", label = "Select group column", 
                         #choices = read_metadata())
                         #choices = names(read_metadata())[!(names(read_metadata()) == "subject")])
                         choices = list())
      )
    )
  })
  
  read_data = eventReactive(input$load_file, {
    if(file_type() == "custom") {
      df = read.csv(input$file$datapath, header = T)
    } else if(!is.null(file_type())) {
      df = parse_file(input$file$datapath, parse_patterns[[file_type()]])
    }
    df = df %>%
      prettify_data(create_param_df(), file_type())
    return(df)
  }, ignoreNULL = T)
  
  read_metadata = reactive({
    return(read.csv(input$metadata$datapath))
  })

  get_subjects = reactive({
    return(read_data()$subject %>% unique())
  })
  
  output$output_field = renderPrint({ 
    list(
      create_param_df(),
      get_subjects(),
      read_data()
    )
  })
  
  data_preview = eventReactive(input$preview_data, {
    return(
      tagList(
        hr(),
        h4("Data Preview"),
        renderTable(head(read_data()), 
                    striped = T)
      )
    )
  })

  output$preview = renderUI({
    data_preview()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
