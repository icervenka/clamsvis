output$daily_grouped_plot <- renderPlot({
  
  
  # TODO works, but there is some json error Input to asJSON(keep_vec_names=TRUE) is a named vector.
  selected_aggregation = "t720"
  selected_parameter = input$select_parameter
  selected_cumulative = input$select_cumulative
  
  group_df = parse_group_inputs(input)
  
  if(dim(group_df)[1] > 0) {
    
    aggregated_df = map2_dfr(data_nest$cropped, 
                             data_nest$subject,
                             .f = aggregate_parameter, 
                             aggdf, 
                             selected_parameter, 
                             selected_aggregation, 
                             aggregate_by(selected_parameter),
                             ifelse(selected_cumulative == "2", TRUE, FALSE))
    
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