output$daily_individual_plot <- renderPlot({

  #TODO remove fasting-refeeding
  # select checkboxes for days and nights for aggregation
  num_parameters = counter$n
  selected_parameters = lapply(1:num_parameters, function(x) {input[[paste0("select_parameter_", x)]]}) %>% unlist(recursive = TRUE)
  
  aggregated_df = map_dfr(selected_parameters, 
                          ~ aggregate_parameter(global_vars$data_agg, 
                                                "t720", 
                                                .x))
  aggregated_df$param = factor(aggregated_df$param, levels = unique(aggregated_df$param))
  
  aggregated_df %>%
    group_by(light, subject) %>% 
    ggplot(aes(x = subject,y = value, fill = subject)) + 
    stat_summary(fun.data = min.mean.sd.max,  geom = "boxplot") +
    plot_jitter(input$display_points) + 
    plot_facets(length(selected_parameters) + 2, "param ~ light")
})


output$daily_individual_plot_render <- renderUI({
  plotOutput("daily_individual_plot", height = input$plot_height, width = input$plot_width)
})