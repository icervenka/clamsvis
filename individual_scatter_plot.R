output$individual_scatter_plot <- renderPlotly({
  
  ai =   aggregate_scatter() %>%
    filter_subjects(input$select_subjects) %>%
    filter_periods(c(input$select_dark, input$select_light))
  
  ai = ai %>% pivot_wider(id_cols = c(subject, interval, light), names_from = param, values_from = mean)

  ai %>%
    ggplot(aes_string(x = input$scatter_x, y = input$scatter_y)) +
    geom_point(aes_string(color = "subject", size = input$scatter_size), alpha = 0.5) + 
    scale_size(range = c(0.1, 5)) +
    plot_facets("2", "~ light")
  
  # ggplotly(p, tooltip = c("subject", "y")) %>%
  #   layout(hovermode = "x",
  #          showlegend = F,
  #          autosize = T)
})

output$individual_scatter_plot_render <- renderUI({
  render_plot("individual_scatter_plot")
})