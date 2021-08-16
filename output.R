output$summary_plot <- renderPlotly({
  print("output::summary_plot")
  if(dim(aggregate_summary())[1] == 0) return()

  rv$current_view = aggregate_summary()

  # aggregate_summary() %>%
  #   dplyr::group_by(param) %>%
  #   do(p = param_graph_panel(.)) %>%
  #   subplot(nrows = NROW(.), shareX = F) %>%
  #   layout(
  #     autosize = T,
  #     yaxis = list(fixedrange = TRUE),
  #     width = input$plot_width,
  #     height = input$plot_height
  #   )

  # TODO facets from function
  p = aggregate_summary() %>%
    ungroup() %>%
    mutate(shift_jitter = as.numeric(as.factor(group)) - 0.35) %>%
    ggplot() +
      geom_boxplot(aes_string(x = input$individual_grouped_select,
                       y = input$select_cumulative,
                       fill = input$individual_grouped_select),
                   outlier.shape = NA,
                   size = 0.5) +
      geom_jitter(aes(x = shift_jitter,
                      y = !!as.symbol(input$select_cumulative),
                      colour = !!as.symbol(input$individual_grouped_select),
                      text = paste("Period:", period)),
                  alpha = 0.7,
                  width = 0.05) +
      facet_grid(param ~ light,
                 scales = "free",
                 labeller = label_both) +
      theme_minimal() +
      theme(legend.position = "none")

  ggplotly(p, tooltip = "text",
           width = input$plot_width,
           height = input$plot_height) %>%
    layout(
      showlegend = T,
      autosize = T
    )
})

output$summary_stat <- DT::renderDT({
  req("1" %in% input$display_stat_table)
  print("output::summary_stat")
  DT::datatable(calculate_pvals(aggregate_summary(), c(param, light)),
    options = list(
      dom = "Brtip",
      buttons = c("csv", "excel")
    ),
    filter = "bottom",
    rownames = FALSE,
    extensions = "Buttons",
    selection = "single"
  ) # %>%
  # DT::formatRound(columns = c("estimate", "statistic", "p.value"), digits=5)
})

output$series_plot <- renderPlotly({
  print("output::series_plot")
  if(dim(aggregate_series())[1] == 0) return()

  rv$current_view = aggregate_series()

  p <- aggregate_series() %>%
    ggplot(aes(
      x = interval,
      y = !!as.symbol(input$select_cumulative)
    ))  +
    geom_line(aes(color = !!as.symbol(input$individual_grouped_select)), size = 0.35) +
    plot_points(
      input$display_point_markers,
      input$individual_grouped_select
    ) +
    plot_errorbars(
      input$display_sd,
      input$individual_grouped_select
    ) +
    plot_facets(aggregate_series())+
    plot_dark_interval_rect(get_dark_interval_rect(
      aggregate_series(),
      input$individual_grouped_select,
      input$select_cumulative
    )) +
    theme_minimal()

  ggplotly(p, tooltip = c("x", "y", "ymin", "ymax"),
           width = input$plot_width,
           height = input$plot_height) %>%
    layout(
      hovermode = "x unified",
      showlegend = T,
      autosize = T
    )
})

output$series_stat <- DT::renderDT({
  req("1" %in% input$display_stat_table)
  print("output::series_stat")
  DT::datatable(calculate_pvals(aggregate_series(), c(param, light, interval)),
                    options = list(
                      dom = "Brtip",
                      buttons = c("csv", "excel")
                    ),
                    filter = "bottom",
                    rownames = FALSE,
                    extensions = "Buttons",
                    selection = "single"
  ) # %>%
  # DT::formatRound(columns = c("estimate", "statistic", "p.value"), digits=5)
})
