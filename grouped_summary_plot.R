filter_group_periods = reactive({
  periods = c(input$select_dark, input$select_light)
  filtered = aggregate_groups() %>%
    dplyr::filter(period %in% periods)
  print(filtered)
  return(filtered)
})

output$daily_grouped_plot <- renderPlotly({
  
  # TODO works, but there is some json error Input to asJSON(keep_vec_names=TRUE) is a named vector.
  # TODO remove fasting-refeeding
  # select checkboxes for days and nights for aggregation
  
  if(dim(global_vars$group_df)[1] > 0) {
    
    ag = filter_group_periods()
    
    # aggregated_df = map_dfr(selected_parameters, 
    #                         ~ aggregate_parameter(global_vars$data_agg, 
    #                                               "t720", 
    #                                               .x))
    # 
    # aggregated_df$param = factor(aggregated_df$param, levels = unique(aggregated_df$param))
    # 
    # group_aggregated_df = merge(aggregated_df, group_df, by = "subject")
    # 
    # global_vars$dark_intervals = group_aggregated_df %>% ungroup %>% filter(light == 0) %>% select(interval) %>% unique %>% pull
    # global_vars$light_intervals = group_aggregated_df %>% ungroup %>% filter(light == 1) %>% select(interval) %>% unique %>% pull
    # 
    # output_df = group_aggregated_df %>%
    #   dplyr::filter(interval %in% input$select_dark | interval %in% input$select_light) %>%
    #   group_by(light, group)
    
    output_df = ag %>% 
      group_by(light, group, period)
    
    global_vars$output_df = output_df
    
    print("daily_group_plot_running")
    
    # output_df %>%
    #   ggplot(aes(x = group, y = value, fill = group)) +
    #   stat_summary(fun.data = min.mean.sd.max,  geom = "boxplot") + 
    #   plot_jitter(input$display_points) + 
    #   plot_facets(length(selected_parameters) + 2, "param ~ light")
    
    panel <- function(x) {
      p1 = x %>%
        dplyr::filter(light == 0) %>%
        # plot_ly(x = ~subject, y = ~mean, color = ~subject,
        #         type = 'violin',
        #         legendgroup = ~subject,
        #         showlegend = F)
        plot_ly(x = ~group, y = ~mean, color = ~group,
                legendgroup = ~group, showlegend = F,
                colors = hue_pal()(length(global_vars$subject_list))) %>%
        add_boxplot(boxmean='sd', notched = T,  text = ~paste("Period:", period),
                    line = list(width = 1), boxpoints = "all", jitter = 0.3, pointpos = -5,
                    marker = list(opacity = 0.75, size = 3),
        )
      
      
      p2 = x %>%
        dplyr::filter(light == 1) %>%
        # plot_ly(x = ~subject, y = ~mean, color = ~subject,
        #         type = 'violin',
        #         legendgroup = ~subject,
        #         showlegend = F)
        plot_ly(x = ~group, y = ~mean, color = ~group,
                legendgroup = ~group, showlegend = F,
                colors = hue_pal()(length(global_vars$subject_list))) %>%
        add_boxplot(boxmean='sd', notched = T,  text = ~paste("Period:", period),
                    line = list(width = 1), boxpoints = "all", jitter = 0.3, pointpos = -5,
                    marker = list(opacity = 0.75, size = 3),
        )
      
      subplot(p1, p2, shareY = T)
    }
    # plot_ly(x = ~subject, y = ~mean, color = ~subject) %>%
    # add_boxplot()
    
    output_df %>% 
      group_by(param) %>%
      do(p = panel(.)) %>%
      subplot(nrows = NROW(.), shareX = F) %>%
      layout(autosize = T,
             yaxis=list(fixedrange=TRUE))
  }
})

output$daily_grouped_plot_render <- renderUI({
  plotlyOutput("daily_grouped_plot", height = input$plot_height, width = input$plot_width)
})

