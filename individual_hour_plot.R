output$individual_hour_plot <- renderPlotly({
  
  ai = aggregate_hour() %>%
    filter_subjects(input$select_subjects) %>%
    filter_periods(c(input$select_dark, input$select_light))
  
  p = ai %>%
    group_by(hour, subject, param) %>%
    summarise(sd = sd(mean), mean = mean(mean), light = dplyr::first(light))  %>%
    ggplot(aes(x = hour, y = mean)) +
    # geom_tile(data = . %>% filter(subject == input$select_subjects[1] & light != 1),
    #           aes(x = (!light)*hour, y = 0 , width = 1, height = Inf),
    #           fill = "grey50", alpha = 0.2, inherit.aes = F) +
    geom_line(aes(color = subject)) +
    plot_points(input$display_points) +
    plot_errorbars(input$display_errorbars, subject) +
    plot_facets(length(rv_filters$parameters))
    
  ggplotly(p, tooltip = c("subject", "y")) %>%
    layout(hovermode = "x",
           showlegend = F,
           autosize = T)
})


output$individual_hour_plot_render <- renderUI({
  plotlyOutput("individual_hour_plot", height = input$plot_height, width = input$plot_width)
})