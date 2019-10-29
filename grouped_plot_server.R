output$group_plot <- renderPlot({
  
  num_parameters = counter$n
  selected_parameters = lapply(1:num_parameters, function(x) {input[[paste0("select_parameter_", x)]]}) %>% unlist(recursive = TRUE)
  
  group_df = parse_group_inputs(input)
  
  if(dim(group_df)[1] > 0) {
    aggregated_df = map_dfr(selected_parameters, 
                            ~ aggregate_parameter(global_vars$data_agg, 
                                                  paste0("t", input$select_aggregation), 
                                                  .x))
    
    aggregated_df$param = factor(aggregated_df$param, levels = unique(aggregated_df$param))

    group_aggregated_df = merge(aggregated_df, group_df, by = "subject")
    print( group_aggregated_df %>%
             group_by(interval, light, group, param) %>% 
             summarise(mean = mean(value), sd = sd(value)))
    
    # pvals = map_dfr(1:max(group_aggregated_df$interval), function(x) {
    #   int_df = group_aggregated_df %>% dplyr::filter(interval == x)
    #   cbind.data.frame(interval = x, tidy(pairwise.t.test(int_df$parameter, int_df$group, p.adjust.method = "none")))
    # })
    # pvals = pvals %>% dplyr::mutate(p.adj = p.adjust(p.value, method = "BH"))
    
    global_vars$max_display_interval = max(aggregated_df$interval)
    
    group_aggregated_df %>%
      group_by(interval, light, group, param) %>% 
      summarise(mean = mean(value), sd = sd(value)) %>%
      dplyr::filter(interval >= input$display_interval[1] & interval <= input$display_interval[2]) %>%
      ggplot(aes(x = interval, y = mean)) + 
      geom_tile(data = . %>% filter(group == "Group1") %>% filter(light != 1),
                aes(x = (!light)*interval, y = 0 , width = 1, height = Inf),
                fill = "grey50", alpha = 0.2, inherit.aes = F) +
      geom_line(aes(colour = group)) + 
      plot_points(input$display_points, group) +
      plot_errorbars(input$display_errorbars) + 
      plot_facets(length(selected_parameters))
    }
})

output$group_plot_render <- renderUI({
  plotOutput("group_plot", height = input$plot_height, width = input$plot_width)
})