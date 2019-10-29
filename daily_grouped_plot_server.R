output$daily_grouped_plot <- renderPlot({
  
  # TODO works, but there is some json error Input to asJSON(keep_vec_names=TRUE) is a named vector.
  # TODO remove fasting-refeeding
  # select checkboxes for days and nights for aggregation
  group_df = parse_group_inputs(input)
  
  num_parameters = counter$n
  
  selected_parameters = lapply(1:num_parameters, function(x) {input[[paste0("select_parameter_", x)]]}) %>% unlist(recursive = TRUE)
  
  if(dim(group_df)[1] > 0) {
    
    aggregated_df = map_dfr(selected_parameters, 
                            ~ aggregate_parameter(global_vars$data_agg, 
                                                  "t720", 
                                                  .x))
    
    aggregated_df$param = factor(aggregated_df$param, levels = unique(aggregated_df$param))
    
    group_aggregated_df = merge(aggregated_df, group_df, by = "subject")
    
    group_aggregated_df %>%
      ggplot(aes(x = group, y = value, fill = group)) +
      stat_summary(fun.data = min.mean.sd.max,  geom = "boxplot") + 
      plot_jitter(input$display_points) + 
      plot_facets(length(selected_parameters) + 2, "param ~ light")
  }
})

output$daily_grouped_plot_render <- renderUI({
  plotOutput("daily_grouped_plot", height = input$plot_height, width = input$plot_width)
})

