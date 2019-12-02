output$grouped_summary_plot <- renderPlotly({
  
  if(dim(rv_data$group_df)[1] > 0) {
   
    ag = aggregate_individuals() %>%
      add_groups() %>%
      filter_periods(c(input$select_dark, input$select_light))
    
    rv_data$current_view = ag
    group_no = length(ag %>% ungroup %>% dplyr::select(group) %>% unique %>% pull)

    # output_df %>%
    #   ggplot(aes(x = group, y = value, fill = group)) +
    #   stat_summary(fun.data = min.mean.sd.max,  geom = "boxplot") + 
    #   plot_jitter(input$display_points) + 
    #   plot_facets(length(selected_parameters) + 2, "param ~ light")
    
    panel <- function(x) {
      p1 = x %>%
        dplyr::filter(light == 0) %>%
        plot_ly(x = ~group, y = ~mean, color = ~group,
                legendgroup = ~group, showlegend = F,
                colors = hue_pal()(group_no)) %>%
        add_boxplot(boxmean='sd', text = ~paste("Period:", period),
                    line = list(width = 1), boxpoints = "all", jitter = 0.3, pointpos = -5,
                    marker = list(opacity = 0.75, size = 3),
        )
      
      p2 = x %>%
        dplyr::filter(light == 1) %>%
        plot_ly(x = ~group, y = ~mean, color = ~group,
                legendgroup = ~group, showlegend = F,
                colors = hue_pal()(group_no)) %>%
        add_boxplot(boxmean='sd', text = ~paste("Period:", period),
                    line = list(width = 1), boxpoints = "all", jitter = 0.3, pointpos = -5,
                    marker = list(opacity = 0.75, size = 3),
        )
      
      subplot(p1, p2, shareY = T)
    }
    
    ag %>% 
      group_by(param) %>%
      do(p = panel(.)) %>%
      subplot(nrows = NROW(.), shareX = F) %>%
      layout(autosize = T,
             yaxis=list(fixedrange=TRUE))
  }
})

output$grouped_summary_plot_render <- renderUI({
  render_plot('grouped_summary_plot')
})

