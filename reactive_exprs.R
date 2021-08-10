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

observeEvent(input$file, {
  print("reactive::file_upload_valid_1")
  rv$file_upload_valid = 0
})

observeEvent(input$load_file, {
  print("reactive::file_upload_valid_2")
  rv$file_upload_valid = 1
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

get_metadata_header = eventReactive(
  {
    input$metadata
  },
  {
    print("reactive::get_metadata_header")
    meta = readr::read_csv(input$metadata$datapath, col_types = readr::cols(.default = "c"))
    return(names(meta)[!(names(meta) == "subject")])
  })

observeEvent(input$update_groups_input, {
  rv$update_groups_input_toggle = 1
  rv$update_groups_file_toggle = 0
})

observeEvent(input$update_groups_file, {
  rv$update_groups_input_toggle = 0
  rv$update_groups_file_toggle = 1
})

# observe(
#   {
#     print("reactive::rvselect_periods")
#     if(is.null(input$select_dark)) {
#       rv$selected_periods_dark = NULL #get_dark_periods()
#     } else {
#       rv$selected_periods_dark = input$select_dark
#     }
#
#     if(is.null(input$select_light)) {
#       rv$selected_periods_light = NULL #get_light_periods()
#     } else {
#       rv$selected_periods_light = input$select_light
#     }
#   },
#   priority = 1
# )

file_type = eventReactive(input$file,
  {
    print("reactive::file_type")
    infer_file_type(input$file$datapath, parse_patterns)
  },
  ignoreNULL = T
)

# processsing of uploaded file ----
custom_colnames = eventReactive(
  {
    input$file
  },
  {
    print("reactive::custom_colnames")
    if (file_type() == "custom") {
      names(read.csv(input$file$datapath))
    }
})

# # light phase start/end changes
# rv_light = reactiveValues(
#   input_start = strptime("06:00:00", "%T"),
#   input_end = strptime("18:00:00", "%T")
# )
#
# observeEvent(
#   {
#     input$light_start
#     input$light_end
#   },
#   {
#     rv_light$input_start = input$light_start
#     rv_light$input_end = input$light_end
#   }
# )

# create parameter dataframe  ----
create_param_df = eventReactive(input$load_file, {
  print("reactive::create_param_df")
  if (file_type() == "custom") {
    required = data.frame(
      colname = c(
        input[["subject"]],
        input[["date_time"]],
        ifelse(input[["light"]] == "infer", "light", input[["light"]])
      ),
      display = c("Subject", "Date-Time", "Light"),
      app = c("subject", "date_time", "light"),
      required = 1,
      aggregate = c(NA, "first", "first"),
      bout = c(FALSE, FALSE, FALSE),
      stringsAsFactors = F
    )

    optional = purrr::map_dfr(1:rv_counters$param_input, function(x) {
      data.frame(
        colname = input[[paste0("parameter_id_", x)]],
        display = ifelse(nchar(input[[paste0("parameter_name_", x)]]) == 0,
          input[[paste0("parameter_id_", x)]],
          input[[paste0("parameter_name_", x)]]
        ),
        app = input[[paste0("parameter_id_", x)]],
        required = 0,
        aggregate = input[[paste0("parameter_agg_", x)]],
        bout = input[[paste0("parameter_bout_", x)]],
        stringsAsFactors = F
      )
    })

    param_df = rbind.data.frame(required, optional)
  } else if (!is.null(file_type())) {
    param_df = read.table(paste0(file_type(), "_colspecs.txt"), sep = "\t", header = T)
  }
  return(param_df)
})

# observe({
#   create_param_df() %>% validate_param_df()
# })


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
