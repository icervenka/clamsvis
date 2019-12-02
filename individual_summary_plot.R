output$individual_summary_plot <- renderPlotly({

  ai = aggregate_individuals() %>%
    filter_periods(c(input$select_dark, input$select_light))
  
  rv_data$current_view = ai
  
  print("daily_plot_running")
  # p = output_df %>% 
  #   ggplot(aes(x = subject, y = mean)) + 
  #   geom_violin(aes(fill = subject, color = subject), trim = F, alpha = 0.5, adjust=2)  +
  # 
  #   #stat_summary(fun.data = min.mean.sd.max,  geom = "boxplot") +
  #   plot_jitter(input$display_points) +
  #   stat_summary(fun.data = mean.sd,
  #                geom="pointrange", shape=21, colour = "black", fill = "white", size = 1) +
  #   plot_facets(length(global_vars$selected_parameters) + 2, "param ~ light")
  # ggplotly(p)
  
  # params = output_df$param %>% unique %>% as.character
  # print(params)
  # 
  # subplot(map(params, function(x) {
  # 
  #   df = output_df %>% dplyr::filter(param == x)
  #     
  #   p1 = df %>%
  #     dplyr::filter(light == 0) %>% 
  #     plot_ly(x = ~subject, y = ~mean, color = ~subject,
  #             type = 'violin',
  #             legendgroup = ~subject,
  #             showlegend = F)
  #   
  #   p2 = df %>%
  #     dplyr::filter(light == 1) %>% 
  #     plot_ly(x = ~subject, y = ~mean, color = ~subject,
  #             type = 'violin',
  #             legendgroup = ~subject,
  #             showlegend = F)
  #     
  #     
  #   subplot(p1, p2, shareY = F, nrows = 1)  
  # }), nrows = length(params), shareX = F)
  
  #colors = hue_pal()(length(global_vars$subject_list))
  
  panel <- function(x) {
    p1 = x %>%
      dplyr::filter(light == 0) %>%
      # plot_ly(x = ~subject, y = ~mean, color = ~subject,
      #         type = 'violin',
      #         legendgroup = ~subject,
      #         showlegend = F)
      plot_ly(x = ~subject, y = ~mean, color = ~subject,
              legendgroup = ~subject, showlegend = F,
              colors = hue_pal()(length(rv_data$subject_list))) %>%
      add_boxplot(boxmean='sd',   text = ~paste("Period:", period),
                  line = list(width = 1), boxpoints = "all", jitter = 0.3, pointpos = -5,
                  marker = list(opacity = 0.75, size = 3),
                  )
   

    p2 = x %>%
      dplyr::filter(light == 1) %>%
      # plot_ly(x = ~subject, y = ~mean, color = ~subject,
      #         type = 'violin',
      #         legendgroup = ~subject,
      #         showlegend = F)
      plot_ly(x = ~subject, y = ~mean, color = ~subject,
              legendgroup = ~subject, showlegend = F,
              colors = hue_pal()(length(rv_data$subject_list))) %>%
      add_boxplot(boxmean='sd',  text = ~paste("Period:", period),
                  line = list(width = 1), boxpoints = "all", jitter = 0.3, pointpos = -5,
                  marker = list(opacity = 0.75, size = 3),
      )
    
    subplot(p1, p2, shareY = T)
  }
  
  ai %>%
    group_by(param) %>%
    do(p = panel(.)) %>%
    subplot(nrows = NROW(.), shareX = F) %>%
    layout(autosize = T,
           yaxis=list(fixedrange=TRUE))
    
  
})

output$individual_summary_plot_render <- renderUI({
  render_plot("individual_summary_plot")
})