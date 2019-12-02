output$individual_series_plot <- renderPlotly({
  # TODO fix y-label to display name of parameter + unit

  ai = aggregate_individuals() %>%
    filter_subjects(input$select_subjects) %>%
    filter_intervals(input$display_interval[1], input$display_interval[2])

  p = ai %>%
    ggplot(aes(x = interval, y = mean)) +
    # geom_tile(data = NULL,
    #           aes(x = interval, y = 0 , width = 1, height = Inf*(!light)*first_subject),
    #           fill = "grey30", alpha = 0.2, inherit.aes = F) +
    geom_line(aes(color = subject), size = 0.35) +
    plot_points(input$display_points) +
    plot_facets(length(rv_filters$parameters))
  
  ggplotly(p, tooltip = c("subject", "y")) %>%
    layout(hovermode = "x",
           showlegend = F,
           autosize = T)
})

output$individual_series_plot_render <- renderUI({
  render_plot("individual_series_plot")
})
