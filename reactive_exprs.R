# add/remove counters ----------------------------------------------------------
# TODO maybe change into modules

# add/remove input params
observeEvent(input$add_param_input_btn, {
  if (rv_counters$param_input < 20) {
    rv_counters$param_input = rv_counters$param_input + 1
  }
})

observeEvent(input$rm_param_input_btn, {
  if (rv_counters$param_input > 1) {
    rv_counters$param_input = rv_counters$param_input - 1
  }
})

# add/remove groups
observeEvent(input$add_group_btn, {
  if (rv_counters$group < 12) {
    rv_counters$group = rv_counters$group + 1
  }
})

observeEvent(input$rm_group_btn, {
  if (rv_counters$group > 2) {
    rv_counters$group = rv_counters$group - 1
  }
})

# add/remove select params
observeEvent(input$add_param_select_btn, {
  if (rv_counters$param_select < 6) {
    rv_counters$param_select = rv_counters$param_select + 1
  }
})

observeEvent(input$rm_param_select_btn, {
  if (rv_counters$param_select > 1) {
    rv_counters$param_select = rv_counters$param_select - 1
  }
})

observe({
  print("reactive::file_rvinterval")
  rv$low_interval = input$display_interval[1]
  rv$high_interval = input$display_interval[2]
}, priority = 1)

observe(
  {
    print("reactive::rvparams")
     selected_params = purrr::map(1:rv_counters$param_select, function(x) {
      input[[paste0("select_parameter_", x)]]
    }) %>%
      purrr::discard(is.null) %>%
      unlist(recursive = TRUE) %>%
      unique()

     if(is.null(selected_params)) {
       rv$selected_params = get_parameters()[1]
     } else {
       rv$selected_params = selected_params
     }

  },
  priority = 1
)

observe(
  {
    print("reactive::rvselect_aggregation")
    if(is.null(input$select_aggregation)) {
      rv$selected_aggregation = "60"
    } else {
      rv$selected_aggregation = input$select_aggregation
    }
  },
  priority = 1
)

observe(
  {
    print("reactive::rv_subjects")
    rv$selected_subjects = input$select_subjects %>%
      purrr::discard(is.null) %>%
      unique()
  },
  priority = 1
)

observe(
  {
    print("reactive::rvselect_interval")
    if(is.null(input$display_interval[1])) {
      rv$selected_interval_low = 1
    } else {
      rv$selected_interval_low = input$display_interval[1]
    }

    if(is.null(input$display_interval[2])) {
      rv$selected_interval_high = get_max_interval()
    } else {
      rv$selected_interval_high = input$display_interval[2]
    }

  },
  priority = 1
)




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
