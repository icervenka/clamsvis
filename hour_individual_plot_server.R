filter_individuals_and_periods = reactive({
  periods = c(input$select_dark, input$select_light)
  filtered = aggregate_individuals() %>%
    dplyr::filter(period %in% periods) %>%
    dplyr::filter(subject %in% input$select_subjects)
  return(filtered)
})

output$hour_individual_plot <- renderPlotly({
  
  ai = filter_individuals_and_periods()
  
  setDT(ai)[, "hour" := hour(ymd_hms(date_time))]
  setDT(ai)[, hour := (hour+input$shift_zt)%%24]
  print(ai)
  
  p = ai %>%
    # dplyr::filter(subject %in% input$select_subjects) %>%
    group_by(hour, subject, param) %>%
    summarise(sd = sd(mean), mean = mean(mean), light = dplyr::first(light))  %>%
    ggplot(aes(x = hour, y = mean)) +
    # geom_tile(data = . %>% filter(subject == input$select_subjects[1] & light != 1),
    #           aes(x = (!light)*hour, y = 0 , width = 1, height = Inf),
    #           fill = "grey50", alpha = 0.2, inherit.aes = F) +
    geom_line(aes(color = subject)) +
    plot_points(input$display_points) +
    plot_errorbars(input$display_errorbars, subject) +
    plot_facets(length(global_vars$selected_parameters))
    
  ggplotly(p, tooltip = c("subject", "y")) %>%
    layout(hovermode = "x",
           showlegend = F,
           autosize = T)
})


output$hour_individual_plot_render <- renderUI({
  plotlyOutput("hour_individual_plot", height = input$plot_height, width = input$plot_width)
})