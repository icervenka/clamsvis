filter_individuals = reactive({
  filtered = aggregate_individuals() %>%
    dplyr::filter(subject %in% input$select_subjects,
                  interval >= input$display_interval[1],
                  interval <= input$display_interval[2])
  return(filtered)
})

output$individual_plot <- renderPlotly({
  # TODO fix y-label to display name of parameter + unit
  # unit might be read from column_specs variable
  
  ai = filter_individuals()
  #print(ai)
  p = ai %>%
    ggplot(aes(x = interval, y = mean)) +
    # geom_tile(data = NULL,
    #           aes(x = interval, y = 0 , width = 1, height = Inf*(!light)*first_subject),
    #           fill = "grey30", alpha = 0.2, inherit.aes = F) +
    geom_line(aes(color = subject), size = 0.35) +
    plot_points(input$display_points) +
    plot_facets(length(global_vars$selected_parameters))
  
  ggplotly(p, tooltip = c("subject", "y")) %>%
    layout(hovermode = "x",
           showlegend = F,
           autosize = T)
})

output$individual_plot_render <- renderUI({
  plotlyOutput("individual_plot", 
             height = global_options$plot_height * global_options$height_multiplier, 
             width = global_options$plot_width)
})





# major_ticks = 720 / input$select_aggregation
# #minor_ticks = major_ticks / 4
# 
# hchart(ai, type = "line", hcaes(x = interval, y = mean, group = subject), zoomtype = "x") %>% 
#   hc_xAxis(
#     startOnTick = TRUE,
#     tickInterval = major_ticks,
#     #minorTickInterval = minor_ticks,
#     alternateGridColor = "#DDDDDD"
#   ) %>%
#   hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
#            {point.y:,.2f}<br/>",
#              shared = TRUE,
#              shadow = FALSE,
#              padding = 2,
#              borderWidth = 1,
#              backgroundColor = "#FFFFFF"
#              ) %>% 
#   hc_plotOptions(
#     line = list(
#       lineWidth = 1,
#       marker = list(
#         radius = 3,
#         symbol = "circle"
#       )
#     )
#   ) %>% 
#   hc_add_theme(hc_theme_smpl()) 
