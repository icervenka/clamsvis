output$daily_individual_plot <- renderPlot({

  aggregated_df = map2_dfr(global_vars$data_subject$cropped, 
                           global_vars$data_subject$subject,
                           .f = aggregate_parameter, 
                           global_vars$aggdf, 
                           input$select_parameter, 
                           "t720", 
                           aggregate_by(input$select_parameter))
  
  p = aggregated_df %>%
    group_by(light, subject) %>% 
    ggplot(aes(x = subject,y = parameter, fill = subject)) + 
    stat_summary(fun.data = min.mean.sd.max,  geom = "boxplot") + 
    facet_wrap(~light, scales = "fixed", labeller = label_both)
  
  if("1" %in% input$display_points) {
    p = p + geom_jitter(shape = 21, colour = "black", fill = "white", size = 3)
  }
  
  if(input$select_comparison_parameter != "None") {
    aggregated_df2 = map2_dfr(global_vars$data_subject$cropped, 
                             global_vars$data_subject$subject,
                             .f = aggregate_parameter, 
                             global_vars$aggdf, 
                             input$select_comparison_parameter, 
                             "t720", 
                             aggregate_by(input$select_comparison_parameter))
    
    aggregated_df = rbind(aggregated_df, aggregated_df2)
    aggregated_df$param = factor(aggregated_df$param, levels = unique(aggregated_df$param))
  }
  
  p = aggregated_df %>%
    group_by(light, subject) %>% 
    ggplot(aes(x = subject,y = parameter, fill = subject)) + 
    stat_summary(fun.data = min.mean.sd.max,  geom = "boxplot") + 
    facet_grid(param ~ light, scales = "free_y", labeller = label_both)
  
  if("1" %in% input$display_points) {
    p = p + geom_jitter(shape = 21, colour = "black", fill = "white", size = 3)
  }
  p
})


output$daily_individual_plot_render <- renderUI({
  plotOutput("daily_individual_plot", height = input$plot_height, width = input$plot_width)
})