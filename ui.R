ui <- fluidPage(
  includeCSS("style.css"),
  titlePanel("CLAMS-VIS"),
  
  sidebarLayout(
    sidebarPanel(
      "",
      
      uiOutput("file_input"),
      
      conditionalPanel(
        condition = "input.tabs1 != 'Download'",
        
        tags$hr(),
        
        uiOutput("select_parameter"),
      
        uiOutput("select_comparison_parameter"),
      
        tags$hr(),
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Individual' | 
                     input.tabs1 == 'Grouped'",
        
        uiOutput("select_aggregation"),
        
        uiOutput("display_interval"),
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Individual' | 
                     input.tabs1 == 'Hour'",
      
        uiOutput("select_subjects")
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Individual' | 
                     input.tabs1 == 'Grouped'",
        
        uiOutput("select_cumulative")
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Individual' | 
                     input.tabs1 == 'Grouped' | 
                     input.tabs1 == 'Daily Individual' | 
                     input.tabs1 == 'Daily Grouped' | 
                     input.tabs1 == 'Hour'",
        
        uiOutput("display_points")
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Grouped' | 
                     input.tabs1 == 'Daily Grouped'",
        
        uiOutput('select_no_groups'),
        
        uiOutput('display_groups'),
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Grouped'",
        
        uiOutput("display_errorbars"),
        
        uiOutput("display_statistics")
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 != 'Download'",
        
        tags$hr(),
        
        uiOutput("plot_width"),
        
        uiOutput("plot_height")
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Download'",      
        p("Due to the rather computationally intensive calculations of all possible aggregations and their subsequent export xlsx format,
         it is currently only possible to select 5 aggregation intervals."),
        h4("The request take several seconds to process after clicking the download button")
      ),
      width = 2
    ),
    mainPanel(
      "",
      
      tabsetPanel(
        id = "tabs1",
        type = "pills",
        
        tabPanel("Individual",
                uiOutput("individual_plot_render")),
        
        tabPanel("Grouped",
                 uiOutput("group_plot_render")),
      
        tabPanel("Daily Individual",
                 uiOutput("daily_individual_plot_render"),
                 uiOutput("daily_individual_plot_render2")),
    
        tabPanel("Daily Grouped",
                 uiOutput("daily_grouped_plot_render")),
  
        tabPanel("Hour",
                 uiOutput("hour_plot_render")),
        
        tabPanel("Circadian",
                 source("temp_ui.R", local = TRUE)[1]),
        
        tabPanel("Sleep",
                 source("temp_ui.R", local = TRUE)[1]),
        
        tabPanel("Movement",
                 source("temp_ui.R", local = TRUE)[1]),
        
        tabPanel("Food",
                 source("temp_ui.R", local = TRUE)[1]),
        
        tabPanel("Drink",
                 source("temp_ui.R", local = TRUE)[1]),
        
        tabPanel("Download", 
                 uiOutput("download"))
        
      ),
      width = 10
    )
  )
)
