observeEvent(input$add_btn, {
  if(rv_filters$counter < 6) rv_filters$counter <- rv_filters$counter + 1
})

observeEvent(input$rm_btn, {
  if(rv_filters$counter > 1) rv_filters$counter <- rv_filters$counter - 1
})

select_parameters = observe({
  print("parameters_running")
  num_parameters = rv_filters$counter
  selected_parameters = lapply(1:num_parameters, function(x) {input[[paste0("select_parameter_", x)]]}) %>% 
    unlist(recursive = TRUE) %>%
    unique
  rv_filters$parameters = selected_parameters
  
})

update_aggregated_filters = function(df) {
  rv_filters$max_interval = max(df$interval)
  rv_filters$dark_periods = df %>% filter(light == 0) %>% select(period) %>% unique %>% pull
  rv_filters$light_periods = df %>% filter(light == 1) %>% select(period) %>% unique %>% pull
}

map_aggregate= function(params, data, time) {
  df = map_dfr(params, 
          ~ aggregate_parameter(data, 
                                paste0("t", time), 
                                .x))
  df$param = factor(df$param, levels = unique(df$param))
  return(df)
}


aggregate_individuals = eventReactive(c(rv_filters$parameters, rv_data$data_agg, input$select_aggregation), ignoreNULL = TRUE, {
  print("individuals_running_1")
  aggregated_df = map_aggregate(rv_filters$parameters, rv_data$data_agg, input$select_aggregation)
  update_aggregated_filters(aggregated_df)
  print("individuals_running_2")
  return(aggregated_df)
})

aggregate_scatter = reactive({
  print("scatter_running")
  aggregated_df = map_aggregate(rv_data$parameters, rv_data$data_agg, input$select_aggregation)
  update_aggregated_filters(aggregated_df)
  return(aggregated_df)
})

aggregate_hour = reactive({
  print("hour_running")
  aggregated_df = map_aggregate(rv_filters$parameters, rv_data$data_agg, "60")
  setDT(aggregated_df)[, "hour" := hour(ymd_hms(date_time))]
  setDT(aggregated_df)[, "hour" := (hour+input$shift_zt)%%24]
  update_aggregated_filters(aggregated_df)
  return(aggregated_df)
})

parse_groups = observe({
  group_df = parse_group_inputs(input)
  rv_data$group_df = group_df
  print("parse_groups_running")
})

add_groups = function(df) {
  if(dim(rv_data$group_df)[1] > 0) {
    print("groups_running")
    ag = merge(df, rv_data$group_df, by = "subject")
    
    ag =  ag %>%
      group_by_at(vars(-subject, -date_time, -mean)) %>% 
      summarise(sd = sd(mean), mean = mean(mean))
    print(ag)
  }
  return(ag)
}

filter_subjects = function(df, subjects) {
  df %>%
    dplyr::filter(subject %in% subjects)
}

filter_intervals = function(df, min, max) {
  df %>% dplyr::filter(interval >= min, interval <= max)
}

filter_periods = function(df, periods) {
  df %>% dplyr::filter(period %in% periods)
}

render_plot = function(out_plot) {
  plotlyOutput(out_plot) #,
               # height = input$plot_height * rv_options$height_multiplier, 
               # width = input$plot_width)
}
