output$daily_individual_plot <- renderPlot({

  num_parameters = counter$n
  selected_parameters = lapply(1:num_parameters, function(x) {input[[paste0("select_parameter_", x)]]}) %>% unlist(recursive = TRUE)
  
  aggregated_df = map_dfr(selected_parameters, 
                          ~ aggregate_parameter(global_vars$data_agg, 
                                                "t720", 
                                                .x))
  aggregated_df$param = factor(aggregated_df$param, levels = unique(aggregated_df$param))
  
  global_vars$dark_intervals = aggregated_df %>% filter(light == 0) %>% select(interval) %>% unique %>% pull
  global_vars$light_intervals = aggregated_df %>% filter(light == 1) %>% select(interval) %>% unique %>% pull
  
  output_df = aggregated_df %>%
    dplyr::filter(interval %in% input$select_dark | interval %in% input$select_light) %>%
    group_by(light, subject)
  
  global_vars$output_df = output_df
  
  
  output_df %>% 
    ggplot(aes(x = subject,y = value, fill = subject)) + 
    stat_summary(fun.data = min.mean.sd.max,  geom = "boxplot") +
    plot_jitter(input$display_points) + 
    plot_facets(length(selected_parameters) + 2, "param ~ light")
})


output$daily_individual_plot_render <- renderUI({
  plotOutput("daily_individual_plot", height = input$plot_height, width = input$plot_width)
})