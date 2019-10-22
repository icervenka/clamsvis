output$individual_plot <- renderPlot({
  # calculate aggregated data frame for selected parameter and time
  
  selected_aggregation = paste0("t", input$select_aggregation)
  selected_parameter = input$select_parameter
  selected_cumulative = input$select_cumulative
  # selected_aggregation = "t60"
  # selected_parameter = "vo2"
  # selected_cumulative = FALSE
  
  aggregated_df = map2_dfr(data_nest$cropped, 
                           data_nest$subject,
                           .f = aggregate_parameter, 
                           aggdf, 
                           selected_parameter, 
                           selected_aggregation, 
                           aggregate_by(selected_parameter),
                           ifelse(selected_cumulative == "2", TRUE, FALSE))
  
  global_vars$max_display_interval = max(aggregated_df$interval)
  
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