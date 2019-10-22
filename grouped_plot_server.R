output$group_plot <- renderPlot({
  
  selected_aggregation = paste0("t", input$select_aggregation)
  selected_parameter = input$select_parameter
  selected_cumulative = input$select_cumulative
  
  group_df = parse_group_inputs(input)
  
  if(dim(group_df)[1] > 0) {

    aggregated_df = map2_dfr(data_nest$cropped, 
                             data_nest$subject,
                             .f = aggregate_parameter, 
                             aggdf, 
                             selected_parameter, 
                             selected_aggregation, 
                             aggregate_by(selected_parameter),
                             ifelse(selected_cumulative == "2", TRUE, FALSE))
    
    group_aggregated_df = merge(aggregated_df, group_df, by = "subject")
    
    # pvals = map_dfr(1:max(group_aggregated_df$interval), function(x) {
    #   int_df = group_aggregated_df %>% dplyr::filter(interval == x)
    #   cbind.data.frame(interval = x, tidy(pairwise.t.test(int_df$parameter, int_df$group, p.adjust.method = "none")))
    # })
    # pvals = pvals %>% dplyr::mutate(p.adj = p.adjust(p.value, method = "BH"))
    
    #group_aggregated_df = group_means(aggregated_df, group_list)
    
    p = group_aggregated_df %>%
      group_by(interval, light, group) %>% 
      summarise(mean = mean(parameter), sd = sd(parameter)) %>%
      dplyr::filter(interval >= input$display_interval[1] & interval <= input$display_interval[2]) %>%
      ggplot(aes(x = interval, y = mean)) + 
      geom_line(aes(colour = group)) +
      theme_bw()
    
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