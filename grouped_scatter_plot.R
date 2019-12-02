output$grouped_scatter_plot <- renderPlotly({
  
  if(dim(rv_data$group_df)[1] > 0) {
    ag =  aggregate_scatter() %>%
      add_groups() %>%
      filter_periods(c(input$select_dark, input$select_light))
    
    ag = ag %>% pivot_wider(id_cols = c(group, interval, light), names_from = param, values_from = mean)
    print(ag)
    
    ag %>%
      ggplot(aes_string(x = input$scatter_x, y = input$scatter_y)) +
      geom_point(aes_string(color = "group", size = input$scatter_size), alpha = 0.5) + 
      scale_size(range = c(0.1, 5)) +
      plot_facets("2", "~ light")
    
    # ggplotly(p, tooltip = c("subject", "y")) %>%
    #   layout(hovermode = "x",
    #          showlegend = F,
    #          autosize = T)
  
  }
})

output$grouped_scatter_plot_render <- renderUI({
  render_plot("grouped_scatter_plot")
})