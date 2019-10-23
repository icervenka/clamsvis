output$hour_plot <- renderPlot({
  byh = map2_dfr(global_vars$data_subject$cropped,  global_vars$data_subject$subject, function(x, y, aggregation, parameter = "vo2") {
    param = parameter
    tm = aggregate(x %>% dplyr::select(date_time, light), aggregation %>% dplyr::select(t60), first)
    agg = aggregate(x %>% dplyr::select(parameter), aggregation %>% dplyr::select(t60), mean)
    names(agg) = c("t60", "parameter")
    tm['subject'] = y
    tm['hour'] = hour(dmy_hms(tm$date_time))
    byhour = data.frame(merge(tm, agg), param = param)
  }, aggregation = global_vars$aggdf, input$select_parameter)
  
  if(input$select_comparison_parameter != "None") {
    byh2 = map2_dfr(global_vars$data_subject$cropped,  global_vars$data_subject$subject, function(x, y, aggregation, parameter = "vo2") {
      param = parameter
      tm = aggregate(x %>% dplyr::select(date_time, light), aggregation %>% dplyr::select(t60), first)
      agg = aggregate(x %>% dplyr::select(parameter), aggregation %>% dplyr::select(t60), mean)
      names(agg) = c("t60", "parameter")
      tm['subject'] = y
      tm['hour'] = hour(dmy_hms(tm$date_time))
      byhour = data.frame(merge(tm, agg), param = param)
    }, aggregation = global_vars$aggdf, input$select_comparison_parameter)
    
    byh = rbind(byh, byh2)
    byh$param = factor(byh$param, levels = unique(byh$param))
    print(head(byh))
    print(tail(byh))
  }
  
  p = byh %>%
    dplyr::filter(subject %in% input$select_subjects) %>%
    group_by(hour, subject, param) %>%
    summarise(mean = mean(parameter), sd = sd(parameter))  %>%
    ggplot(aes(x = hour, y = mean)) +
    geom_line(aes(color = subject)) + 
    geom_ribbon(aes(ymin = mean-sd, ymax = mean+sd, fill = subject), alpha = 0.075)
  
  if("1" %in% input$display_points) {
    p = p + geom_point(aes(color = subject))
  }
  
  if(input$select_comparison_parameter != "None") {
    p = p + facet_grid(param ~ . ,scales = "free_y", labeller = label_both)
  }
  p
})


output$hour_plot_render <- renderUI({
  plotOutput("hour_plot", height = input$plot_height, width = input$plot_width)
})