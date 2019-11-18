output$hour_grouped_plot <- renderPlot({
  
  group_df = parse_group_inputs(input)
  
  num_parameters = counter$n
  
  selected_parameters = lapply(1:num_parameters, function(x) {input[[paste0("select_parameter_", x)]]}) %>% unlist(recursive = TRUE)
  
  if(dim(group_df)[1] > 0) {
  
    aggregated_df = map_dfr(selected_parameters, 
                            ~ aggregate_parameter(global_vars$data_agg, 
                                                  "t60", 
                                                  .x))
    
    setDT(aggregated_df)[, "hour" := hour(dmy_hms(date_time))]
    setDT(aggregated_df)[, hour := (hour+input$shift_zt)%%24]
    
    aggregated_df$param = factor(aggregated_df$param, levels = unique(aggregated_df$param))
    
    group_aggregated_df = merge(aggregated_df, group_df, by = "subject")
    
    group_aggregated_df %>%
      group_by(hour, group, param) %>%
      summarise(mean = mean(value), sd = sd(value), light = dplyr::first(light))  %>%
      ggplot(aes(x = hour, y = mean)) +
      geom_tile(data = . %>% filter(group == "Group1" & light != 1),
                aes(x = (!light)*hour, y = 0 , width = 1, height = Inf),
                fill = "grey50", alpha = 0.2, inherit.aes = F) +
      geom_line(aes(color = group)) +
      plot_points(input$display_points, aes_colour = group) +
      plot_errorbars(input$display_errorbars, group) +
      plot_facets(length(selected_parameters))
  }
})


output$hour_grouped_plot_render <- renderUI({
  plotOutput("hour_grouped_plot", height = input$plot_height, width = input$plot_width)
})