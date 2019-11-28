ui <- fluidPage(
  titlePanel("CLAMS-VIS"),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  sidebarLayout(
    sidebarPanel(
      "",
      
      uiOutput("file_input"),
      
      conditionalPanel(
        condition = "input.tabs1 != 'Download'",
        
        tags$hr(),
      
        uiOutput("textbox_ui"),
        
        fluidRow(
          column(
            align = 'right',
            actionButton("add_btn", "Add"),
            width = 2
          ),
          column(
            align = 'right',
            actionButton("rm_btn", "Remove "),
            width = 2
          )
        ),
        
        tags$hr(),
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Individual - Series' |             
                     input.tabs1 == 'Individual - Hour'",
        
        uiOutput("select_subjects")
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Individual - Series' | 
                     input.tabs1 == 'Grouped - Series' | 
                     input.tabs1 == 'Individual - Summary' | 
                     input.tabs1 == 'Grouped - Summary'",
        
        uiOutput("select_aggregation")
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Individual - Series' | 
                     input.tabs1 == 'Grouped - Series'",
        
        uiOutput("display_interval")
        
      ),
      
      
      conditionalPanel(
        condition = "input.tabs1 == 'Individual - Bout' |
                     input.tabs1 == 'Grouped - Bout'",
        
        uiOutput("bout_aggregation"),
        
        uiOutput("bout_mincount"),
        
        uiOutput('bout_update'),
        
        tags$hr(),
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Individual - Summary' | 
                     input.tabs1 == 'Grouped - Summary' | 
                     input.tabs1 == 'Individual - Hour'",
        
        uiOutput("select_dark"),
        
        uiOutput("select_light")
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Individual - Hour' |
                     input.tabs1 == 'Grouped - Hour'",
        
        uiOutput("shift_zt")
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Grouped - Series' | 
                     input.tabs1 == 'Grouped - Summary' |
                     input.tabs1 == 'Grouped - Hour' |
                     input.tabs1 == 'Grouped - Bout'",
        
        uiOutput('select_no_groups'),
        
        uiOutput('display_groups')
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Individual - Series' | 
                     input.tabs1 == 'Grouped - Series'",
        
        uiOutput("select_cumulative")
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Individual - Series' | 
                     input.tabs1 == 'Grouped - Series' | 
                     input.tabs1 == 'Individual - Summary' | 
                     input.tabs1 == 'Grouped - Summary' | 
                     input.tabs1 == 'Individual - Bout' |
                     input.tabs1 == 'Grouped - Bout' |
                     input.tabs1 == 'Individual - Hour' |
                     input.tabs1 == 'Grouped - Hour'",
        
        uiOutput("display_points")
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Individual - Hour' |
                     input.tabs1 == 'Grouped - Series' | 
                     input.tabs1 == 'Grouped - Hour'",
        
        uiOutput("display_errorbars"),
        
        # uiOutput("display_statistics")
        
      ),
      
      conditionalPanel(
        condition = "input.tabs1 == 'Individual - Summary' | 
                     input.tabs1 == 'Grouped - Summary'",
        
        uiOutput("download_view")
        
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
        
        tabPanel("Individual - Series",
                 uiOutput("individual_plot_render")),
        
        tabPanel("Individual - Summary",
                 uiOutput("daily_individual_plot_render")),
        
        # tabPanel("Individual - Bout",
        #          uiOutput("bout_individual_plot_render")),
        
        tabPanel("Individual - Hour",
                 uiOutput("hour_individual_plot_render")),
        
        tabPanel("Grouped - Series",
                 uiOutput("group_plot_render")),
        
        tabPanel("Grouped - Summary",
                 uiOutput("daily_grouped_plot_render")),
        
        # tabPanel("Grouped - Bout",
        #          uiOutput("bout_grouped_plot_render")),
        
        tabPanel("Grouped - Hour",
                 uiOutput("hour_grouped_plot_render")),
        
        # tabPanel("Circadian",
        #          source("temp_ui.R", local = TRUE)[1]),
        # 
        # tabPanel("Sleep",
        #          source("temp_ui.R", local = TRUE)[1]),
        # 
        # tabPanel("Movement",
        #          source("temp_ui.R", local = TRUE)[1]),
        # 
        # tabPanel("Food",
        #          source("temp_ui.R", local = TRUE)[1]),
        # 
        # tabPanel("Drink",
        #          source("temp_ui.R", local = TRUE)[1]),
        # 
        tabPanel("Download", 
                 uiOutput("download"))
        
      ),
      width = 10
    )
  )
)
