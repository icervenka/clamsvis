output$grouped_hour_plot <- renderPlotly({
  
  if(dim(rv_data$group_df)[1] > 0) {
  
    # change to add hours
    ag = aggregate_hour() %>%
      add_groups() %>%
      filter_periods(c(input$select_dark, input$select_light))

    p = ag %>%
      group_by(hour, group, param) %>%
      summarise(sd = sd(mean), mean = mean(mean),light = dplyr::first(light))  %>%
      ggplot(aes(x = hour, y = mean)) +
      # geom_tile(data = . %>% filter(group == "Group1" & light != 1),
      #           aes(x = (!light)*hour, y = 0 , width = 1, height = Inf),
      #           fill = "grey50", alpha = 0.2, inherit.aes = F) +
      geom_line(aes(color = group)) +
      plot_points(input$display_points, aes_colour = group) +
      plot_errorbars(input$display_errorbars, group) +
      plot_facets(length(rv_filters$parameters))
    
    ggplotly(p, tooltip = c("group", "y")) %>%
      layout(hovermode = "x",
             showlegend = F,
             autosize = T)
  }
})


output$grouped_hour_plot_render <- renderUI({
  plotlyOutput("grouped_hour_plot", height = input$plot_height, width = input$plot_width)
})