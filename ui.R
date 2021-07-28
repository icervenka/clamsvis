### ui -------------------------------------------------------------------------
ui = fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("CLAMS-VIS"),
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  sidebarLayout(
    sidebarPanel(
      "",
      conditionalPanel(
        condition = "input.tabs_main == 'Load Files'",
        uiOutput("example_files")
      ),
      conditionalPanel(
        condition = "input.tabs_main != 'Download' &
                     input.tabs_main != 'Load Files' &
                     input.tabs_main != 'Info'",
        uiOutput("individual_grouped_select"),
      ),
      conditionalPanel(
        condition = "input.tabs_main != 'Download' &
                     input.tabs_main != 'Load Files' &
                     input.tabs_main != 'Summary' &
                     input.tabs_main != 'Info'",

        # in summary all subjects can be displayed at the same time or removed
        # by clicking on plotly plot
        uiOutput("select_subjects")
      ),
      conditionalPanel(
        condition = "input.tabs_main != 'Download' &
                     input.tabs_main != 'Scatterplot' &
                     input.tabs_main != 'Activity' &
                     input.tabs_main != 'Load Files' &
                     input.tabs_main != 'Info'",
        tags$hr(),
        uiOutput("parameter_select")
      ),
      conditionalPanel(
        condition = "input.tabs_main == 'Summary' |
                     input.tabs_main == 'Time Series' |
                     input.tabs_main == 'Scatterplot'",
        uiOutput("select_aggregation")
      ),
      conditionalPanel(
        condition = "input.tabs_main == 'Scatterplot'",
        uiOutput("scatter_controls")
      ),
      conditionalPanel(
        condition = "input.tabs_main == 'Time Series'",
        uiOutput("display_interval")
      ),
      conditionalPanel(
        condition = "input.tabs_main == 'Time Series'",
        uiOutput("select_cumulative")
      ),
      conditionalPanel(
        condition = "input.tabs_main == 'Activity'",
        uiOutput("activity_controls")
      ),
      conditionalPanel(
        condition = "input.tabs_main == 'Summary' |
                     input.tabs_main == 'Scatterplot' |
                     input.tabs_main == 'Circadian'",
        uiOutput("select_phase")
      ),
      conditionalPanel(
        condition = "input.tabs_main == 'Summary' |
                     input.tabs_main == 'Time Series' |
                     input.tabs_main == 'Circadian'",
        uiOutput("display_stat_header")
      ),
      conditionalPanel(
        condition = "input.tabs_main == 'Time Series' |
                     input.tabs_main == 'Circadian'",
        uiOutput("display_sd")
      ),
      conditionalPanel(
        condition = "input.tabs_main == 'Summary' |
                     input.tabs_main == 'Time Series' |
                     input.tabs_main == 'Circadian'",
        uiOutput("display_stat_table")
      ),
      conditionalPanel(
        condition = "input.tabs_main == 'Circadian'",
        uiOutput("shift_zt")
      ),
      conditionalPanel(
        condition = "input.tabs_main != 'Load Files' &
                     input.tabs_main != 'Download' &
                     input.tabs_main != 'Info'",
        uiOutput("download_view")
      ),
      conditionalPanel(
        condition = "input.tabs_main == 'Time Series' |
                     input.tabs_main == 'Circadian'",
        uiOutput("display_point_markers")
      ),
      conditionalPanel(
        condition = "input.tabs_main != 'Load Files' &
                     input.tabs_main != 'Download' &
                     input.tabs_main != 'Info'",
        tags$hr(),
        uiOutput("plot_width"),
        uiOutput("plot_height")
      ),
      conditionalPanel(
        condition = "input.tabs_main == 'Download'",
        p("Due to the rather computationally intensive
        calculations of all possible aggregations and their
        subsequent export xlsx format, it is currently only
          possible to select 5 aggregation intervals."),
        h4("The request take several seconds to
           process after clicking the download button")
      ),
      width = 3
    ),
    mainPanel(
      "",
      tabsetPanel(
        id = "tabs_main",
        type = "tabs",
        tabPanel(
          "Load Files",
          fluidRow(
            br(),
            column(
              12,
              fileInput("file", label = NULL)
            )
          ),
          uiOutput("custom_file_specs"),
          fluidRow(
            column(
              3,
              actionButton("load_file",
                "Load",
                width = "100%",
                class = "btn-primary"
              )
            )
          ),
          br(),
          uiOutput("render_group_info"),
          fluidRow(column(12, uiOutput("preview")))
        ),
        tabPanel(
          "Summary",
          uiOutput('render_summary_plot'),
          #plotlyOutput("summary_plot"),
          DT::DTOutput("summary_stat")
        ),
        tabPanel(
          "Time Series",
          uiOutput('render_series_plot'),
          #plotlyOutput("series_plot"),
          DT::DTOutput("series_stat")
        ),
        tabPanel(
          "Scatterplot",
            uiOutput('render_scatter_plot'),
            #plotlyOutput("scatter_plot"),
            h4("Correlation table"),
            DT::DTOutput("correlations")
        ),
        tabPanel(
          "Activity",
          uiOutput('render_activity_plot'),
          #plotlyOutput("activity_plot"),
          DT::DTOutput("activity_table")
        ),
        tabPanel(
          "Circadian",
          uiOutput('render_circadian_plot'),
          #plotlyOutput("circadian_plot"),
          DT::DTOutput("circadian_stat")
        ),
        tabPanel(
          "Download",
          uiOutput("download")
        ),
        tabPanel(
          "Info",
          uiOutput("info")
        )
      ),
      width = 9
    )
  )
)
