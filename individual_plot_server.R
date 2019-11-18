output$individual_plot <- renderPlot({
  
  num_parameters = counter$n
  selected_parameters = lapply(1:num_parameters, function(x) {input[[paste0("select_parameter_", x)]]}) %>% 
    unlist(recursive = TRUE) %>%
    unique
  
  aggregated_df = map_dfr(selected_parameters, 
                          ~ aggregate_parameter(global_vars$data_agg, 
                                                paste0("t", input$select_aggregation), 
                                                .x))
  aggregated_df$param = factor(aggregated_df$param, levels = unique(aggregated_df$param))
  
  #plot for individuals
  # TODO fix y-label to display name of parameter + unit
  # unit might be read from column_specs variable
  
  global_vars$max_display_interval = max(aggregated_df$interval)
  
  aa = aggregated_df %>%
    dplyr::filter(subject %in% input$select_subjects) %>%
    dplyr::mutate(first_subject = case_when(subject == input$select_subjects[1] ~ 1, 
                                            subject != input$select_subjects[1] ~ 0)) %>%
    dplyr::filter(interval >= input$display_interval[1] & interval <= input$display_interval[2])
  
  aa %>%
    ggplot(aes(x = interval, y = value, color = subject)) + 
    # geom_tile(data = . %>% filter(subject == input$select_subjects[1]) %>% filter(light != 1),
    #           aes(x = (!light)*interval, y = 0 , width = 1, height = Inf),
    #           fill = "grey50", alpha = 0.2, inherit.aes = F) +
    geom_tile(data = NULL,
              aes(x = interval, y = 0 , width = 1, height = Inf*(!light)*first_subject),
              fill = "grey50", alpha = 0.2, inherit.aes = F) +
    geom_line() + 
    plot_points(input$display_points) +
    plot_facets(length(selected_parameters))
})

output$individual_plot_render <- renderUI({
  if(is.null(input$plot_height)) {
    input_height = 500
  } else {
    input_height = input$plot_height
  }
  
  if(is.null(input$plot_width)) {
    input_width = 750
  } else {
    input_width = input$plot_width
  }
  
  plotOutput("individual_plot", height = input_height, width = input_width)
})
