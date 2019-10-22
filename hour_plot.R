output$hour_plot <- renderPlot({
  byh = map2_dfr(data_nest$cropped,  data_nest$subject, function(x, y, parameter = "vo2") {
    tm = aggregate(x %>% dplyr::select(date_time, light), aggdf %>% dplyr::select(t60), first)
    agg = aggregate(x %>% dplyr::select(parameter), aggdf %>% dplyr::select(t60), mean)
    names(agg) = c("t60", "parameter")
    tm['subject'] = y
    tm['hour'] = hour(dmy_hms(tm$date_time))
    byhour = as.data.frame(merge(tm, agg))
  }, input$select_parameter)
  
  p = byh %>%
    dplyr::filter(subject %in% input$select_subjects) %>%
    group_by(hour, subject) %>%
    summarise(mean = mean(parameter), sd = sd(parameter))  %>%
    ggplot(aes(x = hour, y = mean)) +
    geom_line(aes(color = subject)) + 
    geom_ribbon(aes(ymin = mean-sd, ymax = mean+sd, fill = subject), alpha = 0.075) +
    theme_bw()
  
  if("1" %in% input$display_points) {
    p = p + geom_point(aes(color = subject))
  }
  p
})


output$hour_plot_render <- renderUI({
  plotOutput("hour_plot", height = input$plot_height, width = input$plot_width)
})