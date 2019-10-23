output$daily_grouped_plot <- renderPlot({
  
  # TODO works, but there is some json error Input to asJSON(keep_vec_names=TRUE) is a named vector.
  group_df = parse_group_inputs(input)
  
  if(dim(group_df)[1] > 0) {
    
    aggregated_df = map2_dfr(global_vars$data_subject$cropped, 
                             global_vars$data_subject$subject,
                             .f = aggregate_parameter, 
                             global_vars$aggdf, 
                             input$select_parameter, 
                             "t720", 
                             aggregate_by(input$select_parameter),
                             ifelse(input$select_cumulative == "2", TRUE, FALSE))
    
    group_aggregated_df = merge(aggregated_df, group_df, by = "subject")
    
    p = group_aggregated_df %>%
      ggplot(aes(x = group, y = parameter, fill = group)) +
      stat_summary(fun.data = min.mean.sd.max,  geom = "boxplot") + 
      facet_wrap(~light, scales = "fixed", labeller = label_both)
    
    if("1" %in% input$display_points) {
      p = p + geom_jitter(shape = 21, colour = "black", fill = "white", size = 3)
    }
    p
    
  } else {
    ggplot() + theme_void()
  }
})

output$daily_grouped_plot_render <- renderUI({
  plotOutput("daily_grouped_plot", height = input$plot_height, width = input$plot_width)
})

