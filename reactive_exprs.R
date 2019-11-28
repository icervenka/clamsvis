observeEvent(input$add_btn, {
  if(counter$n < 6) counter$n <- counter$n + 1
})

observeEvent(input$rm_btn, {
  if(counter$n > 1) counter$n <- counter$n - 1
})

select_parameters = observe({
  num_parameters = counter$n
  selected_parameters = lapply(1:num_parameters, function(x) {input[[paste0("select_parameter_", x)]]}) %>% 
    unlist(recursive = TRUE) %>%
    unique
  global_vars$selected_parameters = selected_parameters
  print("parameters_running")
})


aggregate_individuals = reactive({

  aggregated_df = map_dfr(global_vars$selected_parameters, 
                          ~ aggregate_parameter(global_vars$data_agg, 
                                                paste0("t", input$select_aggregation), 
                                                .x))
  aggregated_df$param = factor(aggregated_df$param, levels = unique(aggregated_df$param))

  global_vars$max_display_interval = max(aggregated_df$interval)
  global_vars$dark_intervals = aggregated_df %>% filter(light == 0) %>% select(period) %>% unique %>% pull
  global_vars$light_intervals = aggregated_df %>% filter(light == 1) %>% select(period) %>% unique %>% pull
  print("individuals_running")
  
  global_vars$aggregated_df = aggregated_df
  
  return(aggregated_df)
})


parse_groups = observe({
  group_df = parse_group_inputs(input)
  global_vars$group_df = group_df
  print("parse_groups_running")
})


aggregate_groups = reactive({
  if(dim(global_vars$group_df)[1] > 0) {
    ai = aggregate_individuals()
    print("groups_running")
    ag = merge(ai, global_vars$group_df, by = "subject")
    ag =  ag %>%
      group_by(interval, light, period, group, param) %>% 
      summarise(sd = sd(mean), mean = mean(mean))
  }
  return(ag)
})