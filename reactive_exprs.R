observeEvent(input$add_btn, {
  if(rv_filters$counter < 6) rv_filters$counter <- rv_filters$counter + 1
})

observeEvent(input$rm_btn, {
  if(rv_filters$counter > 1) rv_filters$counter <- rv_filters$counter - 1
})

select_parameters = observe({
  num_parameters = rv_filters$counter
  selected_parameters = lapply(1:num_parameters, function(x) {input[[paste0("select_parameter_", x)]]}) %>% 
    unlist(recursive = TRUE) %>%
    unique
  rv_filters$parameters = selected_parameters
  print("parameters_running")
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


aggregate_individuals = reactive({
  print("individuals_running")
  
  # aggregated_df = map_dfr(rv_filters$parameters, 
  #                         ~ aggregate_parameter(rv_data$data_agg, 
  #                                               paste0("t", input$select_aggregation), 
  #                                               .x))
  # aggregated_df$param = factor(aggregated_df$param, levels = unique(aggregated_df$param))
  
  aggregated_df = map_aggregate(rv_filters$parameters, rv_data$data_agg, input$select_aggregation)
  update_aggregated_filters(aggregated_df)
  return(aggregated_df)
})

aggregate_scatter = reactive({
  print("scatter_running")
  # aggregated_df = map_dfr(rv_data$parameters, 
  #                         ~ aggregate_parameter(rv_data$data_agg, 
  #                                               paste0("t", input$select_aggregation), 
  #                                               .x))
  aggregated_df = map_aggregate(rv_data$parameters, rv_data$data_agg, input$select_aggregation)
  update_aggregated_filters(aggregated_df)
  return(aggregated_df)
})

aggregate_hour = reactive({
  print("hour_running")
  # aggregated_df = map_dfr(rv_filters$parameters, 
  #                         ~ aggregate_parameter(rv_data$data_agg, 
  #                                               paste0("t", "60"), 
  #                                               .x))
  # aggregated_df$param = factor(aggregated_df$param, levels = unique(aggregated_df$param))
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
      # group_by(interval, light, period, group, param) %>% 
      group_by_at(vars(-subject, -date_time, -mean)) %>% 
      summarise(sd = sd(mean), mean = mean(mean))
    print(ag)
  }
  return(ag)
}


# aggregate_groups = reactive({
#   if(dim(rv_data$group_df)[1] > 0) {
#     print("groups_running")
#     ai = aggregate_individuals()
#     ag = merge(ai, rv_data$group_df, by = "subject")
#     ag =  ag %>%
#       group_by(interval, light, period, group, param) %>% 
#       summarise(sd = sd(mean), mean = mean(mean))
#   }
#   return(ag)
# })

# filter_chain_individual_series = reactive({
#   aggregate_individuals() %>%
#     filter_subjects(input$select_subjects) %>%
#     filter_intervals(input$display_interval[1], input$display_interval[2])
# })

# filter_chain_individual_summary = reactive({
#   aggregate_individuals() %>%
#     filter_periods(c(input$select_dark, input$select_light))
# })

# filter_chain_individual_scatter = reactive({
#   aggregate_scatter() %>%
#     filter_subjects(input$select_subjects) %>%
#     filter_periods(c(input$select_dark, input$select_light))
# })

# filter_chain_individual_hour = reactive({
#   aggregate_hour() %>%
#     filter_subjects(input$select_subjects) %>%
#     filter_periods(c(input$select_dark, input$select_light))
# })

# filter_chain_grouped_series = reactive({
#   aggregate_groups() %>%
#     filter_intervals(input$display_interval[1], input$display_interval[2])
# })

# filter_chain_grouped_summary = reactive({
#   aggregate_groups() %>%
#     filter_periods(c(input$select_dark, input$select_light))
# })

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
  plotlyOutput(out_plot,
               height = input$plot_height * rv_options$height_multiplier, 
               width = input$plot_width)
}
