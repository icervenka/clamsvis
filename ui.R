ui <- fluidPage(
  includeCSS("style.css"),
  titlePanel("CLAMS-VIS"),
  
  sidebarLayout(
    sidebarPanel(
      "",
      
      uiOutput("file_input"),
      
      tags$hr(),
      
      uiOutput("select_parameter"),
      
      uiOutput("select_comparison_parameter"),
      
      uiOutput("select_aggregation"),
      
      uiOutput("display_interval"),
      
      uiOutput("select_cumulative"),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Individual'",
        
        uiOutput("select_subjects"),
        
      ),
      
      uiOutput("display_points"),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Grouped'",
        
        uiOutput("display_errorbars"),
        
        uiOutput('select_no_groups'),
        
        uiOutput('display_groups'),
        
        uiOutput("display_statistics"),
      ),
      
      tags$hr(),
      
      uiOutput("plot_width"),
      
      uiOutput("plot_height"),
      
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
      
        tabPanel("Summary",
                 source("temp_ui.R", local = TRUE)[1]),
    
        tabPanel("Group Summary",
                 source("temp_ui.R", local = TRUE)[1]),
  
        tabPanel("Hour",
                 source("temp_ui.R", local = TRUE)[1]),
        
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
                 source("temp_ui.R", local = TRUE)[1])
        
      ),
      width = 10
    )
  )
)
