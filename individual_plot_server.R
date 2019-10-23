output$individual_plot <- renderPlot({
  # calculate aggregated data frame for selected parameter and time
  
  aggregated_df = map2_dfr(global_vars$data_subject$cropped, 
                           global_vars$data_subject$subject,
                           .f = aggregate_parameter, 
                           global_vars$aggdf, 
                           input$select_parameter, 
                           paste0("t", input$select_aggregation), 
                           aggregate_by(input$select_parameter),
                           ifelse(input$select_cumulative == "2", TRUE, FALSE))
  
  # why this doesnt need to be in other functions as well
  # because its reactive?
  
  
  #plot for individuals
  # TODO fix y-label to display name of parameter + unit
  # unit might be read from column_specs variable
  
  # TODO add a comparison graph to compare two variables at the same scale
  # passing multiple paramters to aggregate_paramter and then melting might be a way to go
  
  # TODO dark-light rectangles
  # may return light column from aggregate parameter or another function
  
  if(input$select_comparison_parameter != "None") {
    aggregated_df2 = map2_dfr(global_vars$data_subject$cropped, 
                              global_vars$data_subject$subject,
                              .f = aggregate_parameter, 
                              global_vars$aggdf, 
                              input$select_comparison_parameter, 
                              paste0("t", input$select_aggregation), 
                              aggregate_by(input$select_comparison_parameter),
                              ifelse(input$select_cumulative == "2", TRUE, FALSE))
    
    aggregated_df = rbind(aggregated_df, aggregated_df2)
    aggregated_df$param = factor(aggregated_df$param, levels = unique(aggregated_df$param))
  }
  
  global_vars$max_display_interval = max(aggregated_df$interval)
  
  p = aggregated_df %>%
    dplyr::filter(subject %in% input$select_subjects) %>%
    dplyr::filter(interval >= input$display_interval[1] & interval <= input$display_interval[2]) %>% 
    ggplot(aes(x = interval, y = parameter, color = subject)) + 
    geom_line()
  
  if("1" %in% input$display_points) {
    p = p + geom_point()
  }
  
  if(input$select_comparison_parameter != "None") {
    p = p +  facet_grid(param ~ . ,scales = "free_y", labeller = label_both)
  }
  p
})

output$individual_plot_render <- renderUI({
  plotOutput("individual_plot", height = input$plot_height, width = input$plot_width)
})