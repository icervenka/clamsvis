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

# reactive getters ------
get_subjects = eventReactive(read_data(), {
  print("reactive::get_subjects")
  return(read_data()$subject %>% unique())
})

get_groups = reactive({
    print("reactive::getgroups")
    print(rv$update_groups_input_toggle)
    print(rv$update_groups_file_toggle)
    if(rv$update_groups_input_toggle == 1) {
      group_df = parse_group_inputs(input, rv_counters$group)
    } else if(rv$update_groups_file_toggle == 1) {
      group_df = parse_group_file(input)
    }
    print(group_df)
    return(group_df)
  })

get_parameters = eventReactive({
    phase_align()
    create_param_df()
  }, {
    print("reactive::get_parameters")
    nms = names(phase_align())
    nms = nms[!(nms %in% c("subject", "date_time", "light", "interval", "period"))]
    vals = create_param_df() %>%
      dplyr::filter(app %in% nms)
    p = vals$app
    names(p) = vals$display
    return(p)
  })

get_max_interval = eventReactive(
  {
    get_data_agg_combined()
    rv$selected_aggregation
    get_parameters()
  },
  {
  print("reactive::get_max_interval")
  ag = aggregate_selected_params(
    get_data_agg_combined(),
    rv$selected_aggregation,
    rv$selected_params,
    create_param_df()) %>%
    dplyr::pull(interval) %>%
    max()
  return(ag)
})

get_dark_periods = eventReactive(phase_align(), {
  print("reactive::get_dark_periods")
  phase_align() %>%
    dplyr::filter(light == 0) %>%
    dplyr::pull(period) %>%
    unique()
})

get_light_periods = eventReactive(phase_align(), {
  print("reactive::get_light_periods")
  phase_align() %>%
    dplyr::filter(light == 1) %>%
    dplyr::pull(period) %>%
    unique()
})

get_phase_durations = eventReactive(
  {
    phase_align()
    rv$global_frequency
  },
  {
    print("reactive::get_phase_durations")
    phase_align() %>%
      dplyr::filter(period != max((.) %>% dplyr::pull(period))) %>%
      dplyr::group_by(subject, period) %>%
      dplyr::summarise(duration = dplyr::n() * rv$global_frequency, .groups = "drop") %>%
      dplyr::pull(duration) %>%
      unique()
  }
)

valid_freq = eventReactive(
  {
    rv$global_frequency
    get_phase_durations()
  },
  {
    print("reactive::valid_freq")
    return(get_valid_time_agg(rv$global_frequency, get_phase_durations())$values)
})

# reactive setters ------
set_global_frequency = observeEvent(phase_align(), {
  print("reactive::set_global_freq")
  rv$global_frequency = find_frequencies(read_data(), subject, date_time)
})

# read and process data ------
read_data = eventReactive(input$load_file,
                          {
                            # if(file_type() == "custom") {
                            #   df = read.csv(input$file$datapath, header = T)
                            # } else if(!is.null(file_type())) {
                            #   df = parse_file(input$file$datapath, parse_patterns[[file_type()]])
                            # }
                            print("reactive::read_data")
                            df = parse_file(input$file$datapath, parse_patterns[["clams"]])
                            df = df %>%
                              prettify_data(create_param_df(), file_type())
                            return(df)
                          },
                          ignoreNULL = T
)

phase_align = eventReactive(read_data(), {
  print("reactive::phase_align")
  read_data() %>%
    dplyr::group_by(subject) %>%
    dplyr::mutate(interval = dplyr::row_number()) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      first_night_interval = purrr::map_dbl(data, . %>%
                                              dplyr::filter(light == 0) %>%
                                              dplyr::top_n(1, -interval) %>%
                                              dplyr::select(interval) %>%
                                              as.numeric()),
      no_records = purrr::map_dbl(data, . %>%
                                    dplyr::count() %>%
                                    as.numeric()),
      cropped_records = purrr::map2_dbl(.x = data, .y = first_night_interval, function(x, y) {
        (x %>% dplyr::count() %>% as.numeric()) + 1 - y
      })
    ) %>%
    dplyr::mutate(cropped = purrr::modify2(data, first_night_interval, function(x, y, mm) {
      x %>%
        dplyr::filter(interval >= y & interval <= (mm + y)) %>%
        dplyr::mutate(interval = dplyr::row_number())
    }, mm = min(cropped_records))) %>%
    dplyr::select(subject, cropped) %>%
    tidyr::unnest(cols = c(cropped)) %>%
    dplyr::group_by(subject) %>%
    dplyr::mutate(period = cumsum(c(1, diff(light) != 0)))
})

shinyjs::onclick("preview_data", shinyjs::toggle(id = "prev_div"))

get_data_agg_combined = reactive({
  print("reactive::get_data_agg_combined")
  agg = create_aggregation_df(
    get_phase_durations(),
    rv$global_frequency,
    sum(phase_align()$subject == phase_align()$subject[1])
  )
  return(cbind.data.frame(phase_align(), agg))
})

aggregate_summary = reactive(
  # {
  #   rv$selected_params
  #   get_data_agg_combined()
  #   input$select_aggregation
  #   input$individual_grouped_select
  #   input$select_dark
  #   input$select_light
  #   get_groups()
  # },
  {
    print("reactive::aggregate_summary")
    aggregated_df = aggregate_selected_params(
      get_data_agg_combined(),
      rv$selected_aggregation,
      rv$selected_params,
      create_param_df()
    ) %>%
      add_groups(get_groups()) %>%
      summarise_groups_bool(input$individual_grouped_select) %>%
      filter_periods(c(input$select_dark, input$select_light))
    return(aggregated_df)
  }
)

aggregate_series = reactive(
  # {
  #   rv$selected_params
  #   rv$selected_subjects
  #   get_data_agg_combined()
  #   input$select_aggregation
  #   input$individual_grouped_select
  #   input$display_interval[1]
  #   input$display_interval[2]
  #   get_max_interval()
  #   get_groups()
  #   # req(isTruthy(input$A) | isTruthy(input$B))
  # },
  {
    print("reactive::aggregate_series")
    aggregated_df = aggregate_selected_params(
      get_data_agg_combined(),
      rv$selected_aggregation,
      rv$selected_params,
      create_param_df()
    ) %>%
      add_groups(get_groups()) %>%
      summarise_groups_bool(input$individual_grouped_select,
                            filter = T,
                            input$select_subjects) %>%
      filter_intervals(rv$selected_interval_low, rv$selected_interval_high)
    return(aggregated_df)
  }
)

aggregate_scatter = reactive(
  # {
  #   get_parameters()
  #   get_data_agg_combined()
  #   input$select_aggregation
  # },
  {
    print("reactive::aggregate_scatter")
    aggregated_df = aggregate_selected_params(
      get_data_agg_combined(),
      rv$selected_aggregation,
      get_parameters(),
      create_param_df()
    ) %>%
      add_groups(get_groups()) %>%
      summarise_groups_bool(input$individual_grouped_select,
                            filter = T,
                            rv$selected_subjects) %>%
      filter_periods(c(input$select_dark, input$select_light)) %>%
      tidyr::pivot_wider(
        id_cols = c(input$individual_grouped_select, interval, light, period),
        names_from = param,
        values_from = mean
      )
    return(aggregated_df)
  }
)

aggregate_activity = reactive(
  # {
  #   rv$selected_params
  #   get_data_agg_combined()
  #   input$activity_aggregation
  #   input$activity_mincount
  # },
  {
    print("reactive::aggregate_activity")
    print(input$activity_mincount)

    aggregated_df = aggregate_selected_params(
      get_data_agg_combined(),
      input$activity_aggregation,
      input$select_activity_parameter,
      create_param_df()
    ) %>%
      dplyr::mutate(activity = dplyr::case_when(
        mean < input$activity_mincount ~ "inactive",
        mean >= input$activity_mincount ~ "active"
      )) %>%
      add_groups(get_groups()) %>%
      summarise_groups_bool(input$individual_grouped_select,
                            filter = T,
                            rv$selected_subjects) %>%
      calculate_activity_summary(input$individual_grouped_select)
    return(aggregated_df)
  }
)

aggregate_circadian = reactive(
  # {
  #   rv$selected_params
  #   get_data_agg_combined()
  #   input$select_aggregation
  #   input$shift_zt
  # },
  {
    print("reactive::aggregate_aggregate_circadian")
    aggregated_df = aggregate_selected_params(
      get_data_agg_combined(),
      "60",
      rv$selected_params,
      create_param_df()
    ) %>%
      dplyr::mutate(hour = lubridate::hour(lubridate::ymd_hms(date_time))) %>%
      dplyr::mutate(hour = (hour + input$shift_zt) %% 24) %>%
      add_groups(get_groups()) %>%
      summarise_groups_bool(input$individual_grouped_select,
                            filter = T,
                            rv$selected_subjects) %>%
      filter_periods(c(input$select_dark, input$select_light)) %>%
      dplyr::group_by(hour,
                      !!as.symbol(input$individual_grouped_select),
                      param) %>%
      dplyr::summarise(
        sd = sd(mean),
        mean = mean(mean),
        light = dplyr::first(light),
        .groups = "drop"
      )
    return(aggregated_df)
  }
)

aggregate_circadian_table = reactive(
  # {
  #   rv$selected_params
  #   get_data_agg_combined()
  #   input$select_aggregation
  #   input$shift_zt
  # },
  {
    print("reactive::aggregate_aggregate_circadian_table")
    aggregated_df = aggregate_selected_params(
      get_data_agg_combined(),
      "60",
      rv$selected_params,
      create_param_df()
    ) %>%
      dplyr::mutate(hour = lubridate::hour(lubridate::ymd_hms(date_time))) %>%
      dplyr::mutate(hour = (hour + input$shift_zt) %% 24) %>%
      add_groups(get_groups()) %>%
      summarise_groups_bool(input$individual_grouped_select,
                            filter = T,
                            rv$selected_subjects) %>%
      filter_periods(c(input$select_dark, input$select_light))
    return(aggregated_df)
  }
)

# notifications ------
observeEvent(input$update_groups_input, {
    showNotification("Groups updated",
      duration = 2,
      closeButton = F,
      type = "message"
  )
})

observeEvent(input$update_groups_file, {
  showNotification("Groups updated",
                   duration = 2,
                   closeButton = F,
                   type = "message"
  )
})
