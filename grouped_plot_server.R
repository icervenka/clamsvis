output$group_plot <- renderPlot({
  
  group_df = parse_group_inputs(input)
  
  if(dim(group_df)[1] > 0) {

    aggregated_df = map2_dfr(global_vars$data_subject$cropped, 
                             global_vars$data_subject$subject,
                             .f = aggregate_parameter, 
                             global_vars$aggdf, 
                             input$select_parameter, 
                             paste0("t", input$select_aggregation), 
                             aggregate_by(input$select_parameter),
                             ifelse(input$select_cumulative == "2", TRUE, FALSE))
    
    group_aggregated_df = merge(aggregated_df, group_df, by = "subject")
    
    # pvals = map_dfr(1:max(group_aggregated_df$interval), function(x) {
    #   int_df = group_aggregated_df %>% dplyr::filter(interval == x)
    #   cbind.data.frame(interval = x, tidy(pairwise.t.test(int_df$parameter, int_df$group, p.adjust.method = "none")))
    # })
    # pvals = pvals %>% dplyr::mutate(p.adj = p.adjust(p.value, method = "BH"))
    
    global_vars$max_display_interval = max(aggregated_df$interval)
    
    p = group_aggregated_df %>%
      group_by(interval, light, group) %>% 
      summarise(mean = mean(parameter), sd = sd(parameter)) %>%
      dplyr::filter(interval >= input$display_interval[1] & interval <= input$display_interval[2]) %>%
      ggplot(aes(x = interval, y = mean)) + 
      geom_line(aes(colour = group))
    
    if("1" %in% input$display_points) {
      p = p + geom_point(aes(colour = group))
    }
    if(input$display_errorbars == "2") {
      p = p + geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd, fill = group), alpha = 0.08)
    }
    p
    
    } else {
      ggplot() + theme_void()
    }
})

output$group_plot_render <- renderUI({
  plotOutput("group_plot", height = input$plot_height, width = input$plot_width)
})