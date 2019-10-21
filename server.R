source("scripts.R")

# boxes = c("1001", "1002", "1003", "1004")

boxes = data_nest$subject
# parameters = c("vo2", "vco2", "heat", "rer")
parameters = parameters

df = data_nest

server <- function(input, output) {
  
  values = reactiveValues()
  
  output$file_input <- renderUI({
    fileInput("file", "Upload your file")
  })
  
  output$select_parameter <- renderUI({
    # Create the checkboxes and select them all by default
    selectInput("select_parameter", label = "Select parameter", 
                choices = parameters, 
                selected = 1)
  })
  
  output$select_comparison_parameter <- renderUI({
    # Create the checkboxes and select them all by default
    selectInput("select_comparison_parameter", label = "Select comparison parameter", 
                choices = c("None", parameters), 
                selected = 1)
  })
  
  output$select_aggregation <- renderUI({
    shinyWidgets::sliderTextInput("select_aggregation", "Select aggregation [min]",
                    choices = time_aggregation_values %>% as.character, selected = "60")
  })
  
  output$select_cumulative <- renderUI({
    radioButtons("select_cumulative", label = "Plot",
                 choices = list("Interval data" = 1, "Cumulative data" = 2), 
                 selected = 1)
  })
  
  output$select_subjects <- renderUI({
    # Create the checkboxes and select them all by default
    checkboxGroupInput("select_subjects", "Select Subjects", 
                       choices  = boxes,
                       selected = boxes[1:2])
  })
  
  output$select_no_groups <- renderUI({
    # Create the checkboxes and select them all by default
    numericInput("select_no_groups", label = "Select number of groups", value = 1, min =1, max = 16)
  })
  
   output$display_groups <- renderUI({
    # Create the checkboxes and select them all by default
    print
    map(1:as.integer(input$select_no_groups), function(i) {
      isolate(textInput(paste0("group_no_", i), label = paste0("Group: ", i), value = input[[paste0("group_no_", as.character(i))]]))
    })
  })
  
  output$display_points <- renderUI({
    # Create the checkboxes and select them all by default
    checkboxGroupInput("display_points", "Display additional", 
                       choices  = c("Display points" = 1))
  })
  
  output$display_errorbars <- renderUI({
    # Create the checkboxes and select them all by default
    checkboxGroupInput("display_errorbars", "", 
                       choices  = c("Display error bars" = 1))
  })
  
  output$display_statistics <- renderUI({
    radioButtons("display_statistics", label = "Display statistics",
                 choices = list("none" = 1, "p-values" = 2, "adjusted p-values (BH)" = 3), 
                 selected = 1)
  })
  
  output$display_interval <- renderUI({
    sliderInput("display_interval", label = "Display intervals", min = 1, 
                max = values$max_display_interval, value = c(0, values$max_display_interval), step = 1)
  })
  
  output$plot_width <- renderUI({
    sliderInput("plot_width", label = "Plot width [px]", min = 1000, 
                max = 2000, value = 1500)
  })
  
  output$plot_height <- renderUI({
   sliderInput("plot_height", label = "Plot height [px]", min = 500, 
               max = 1000, value = 750)
  })
  
  output$individual_plot <- renderPlot({
    # calculate aggregated data frame for selected parameter and time
    
    sel_agg = paste0("t", input$select_aggregation)
    aggregated_df = map2_dfr(data_nest$cropped, 
                             data_nest$subject,
                             .f = aggregate_parameter, 
                             aggdf, 
                             input$select_parameter, 
                             sel_agg, 
                             aggregate_by(input$select_parameter),
                             ifelse(input$select_cumulative == "2", TRUE, FALSE))
    
    # rename columns so all the aggregations have the same signature
    colnames(aggregated_df) = c("interval", "parameter", "subject")
    values$max_display_interval = max(aggregated_df$interval)
    
    #plot for individuals
    # TODO fix y-label to display name of parameter + unit
    # unit might be read from column_specs variable
    
    # TODO add a comparison graph to compare two variables at the same scale
    # passing multiple paramters to aggregate_paramter and then melting might be a way to go
    
    # TODO dark-light rectangles
    # may return light column from aggregate parameter or another function
    
    p = aggregated_df %>%
      dplyr::filter(subject %in% input$select_subjects) %>%
      dplyr::filter(interval >= input$display_interval[1] & interval <= input$display_interval[2]) %>% 
      ggplot(aes(x = interval, y = parameter, color = subject)) + 
      geom_line() +
      theme_bw()
    
    if("1" %in% input$display_points) {
      p = p + geom_point()
    }
    p
  })
  
  output$individual_plot_render <- renderUI({
      plotOutput("individual_plot", height = input$plot_height, width = input$plot_width)
  })
  
  output$group_plot <- renderUI({
    no_groups = as.integer(input$select_no_groups)
    
    group_list = lapply(1:no_groups, function(x) {
      input[[paste0("group_no_", x)]]
    })
    group_list[sapply(group_list, is.null)] <- list("")
    
    group_list <- rapply(lapply(group_list, strsplit, ","), str_trim, how = "list") %>%
      lapply(unlist)
    
    
    print(input$select_parameter)
    print(paste0("t", input$select_aggregation))
    aggregated_df = group_aggregate_paramter(data_nest,
                                             aggdf,
                                             input$select_parameter,
                                             paste0("t", input$select_aggregation),
                                             aggregate_by(input$select_parameter))
    
    group_aggregated_df = group_means(aggregated_df, group_list)
    
    p = group_aggregated_df %>%
      dplyr::filter(interval >= input$display_interval[1] & interval <= input$display_interval[2]) %>% 
      ggplot(aes(x = interval, y = mean, color = group)) + 
      geom_line() +
      theme_bw() + 
      geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))
    
  })
  
  output$group_plot_render <- renderUI({
    plotOutput("group_plot", height = input$plot_height, width = input$plot_width)
  })
  
  
  
}
