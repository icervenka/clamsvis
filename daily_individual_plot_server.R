output$daily_individual_plot <- renderPlot({
  aggregated_df = map2_dfr(data_nest$cropped, 
                           data_nest$subject,
                           .f = aggregate_parameter, 
                           aggdf, 
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
  p
})


output$daily_individual_plot_render <- renderUI({
  plotOutput("daily_individual_plot", height = input$plot_height, width = input$plot_width)
})