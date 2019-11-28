filter_groups = reactive({
  filtered = aggregate_groups() %>%
    dplyr::filter(interval >= input$display_interval[1],
                  interval <= input$display_interval[2])
  return(filtered)
})

output$group_plot <- renderPlotly({
  
  if(dim(global_vars$group_df)[1] > 0) {
    
    ag = filter_groups()
    
    # pvals = map_dfr(1:max(group_aggregated_df$interval), function(x) {
    #   int_df = group_aggregated_df %>% dplyr::filter(interval == x)
    #   cbind.data.frame(interval = x, tidy(pairwise.t.test(int_df$parameter, int_df$group, p.adjust.method = "none")))
    # })
    # pvals = pvals %>% dplyr::mutate(p.adj = p.adjust(p.value, method = "BH"))
    
    p = ag %>%
      ggplot(aes(x = interval, y = mean)) + 
      # geom_tile(data = . %>% filter(group == "Group1") %>% filter(light != 1),
      #           aes(x = (!light)*interval, y = 0 , width = 1, height = Inf),
      #           fill = "grey50", alpha = 0.2, inherit.aes = F) +
      geom_line(aes(colour = group), size = 0.35) + 
      plot_points(input$display_points, group) +
      plot_errorbars(input$display_errorbars) + 
      plot_facets(length(global_vars$selected_parameters))
  }
  ggplotly(p, tooltip = c("subject", "group", "y")) %>%
    layout(hovermode = "x",
           showlegend = T,
           autosize = T)
})

output$group_plot_render <- renderUI({
  plotlyOutput("group_plot",
             height = global_options$plot_height * global_options$height_multiplier, 
             width = global_options$plot_width)
})