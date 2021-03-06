### processing of uploaded file ------
infer_file_type = function(file, parse_patterns) {
  print("func::infer_file_type")
  file_patterns = purrr::map(names(parse_patterns), function(x) {
    parse_patterns[[x]][["file_pattern"]]
  }) %>% setNames(names(parse_patterns))

  found_pattern = purrr::map(names(file_patterns), function(x, file_lines) {
    grepl(file_patterns[[x]], file_lines) %>% sum()
  }, file_lines = readLines(file)) %>%
    setNames(names(file_patterns))

  found_pattern_bool = found_pattern > 0

  # TODO I need to catch file type not recognized to display custom form later
  if (sum(found_pattern_bool) == 1) {
    return(which(found_pattern_bool) %>% names())
  } else if (sum(found_pattern_bool) == 0) {
    print("File type not recognized")
    return("custom")
  } else if (sum(found_pattern_bool) > 1) {
    print("More than one type fits the criteria")
    return(NULL)
  } else {
    print("Unknown error.")
    return(NULL)
  }
}

remove_empty_lines = function(file_lines) {
  print("func::remove_empty_lines")
  file_lines[nchar(file_lines) != 0]
}

parse_file = function(filepath, parse_pattern) {
  print("func::parse_file")
  file_lines = remove_empty_lines(readLines(filepath))

  header_line = grep(parse_pattern$header_pattern, file_lines)[1]
  data_start_line = header_line + parse_pattern$data_start_offset
  data_end_line = ifelse(is.null(parse_pattern$data_end_pattern),
    length(file_lines),
    grep(parse_pattern$data_end_pattern, file_lines)[1] - 1
  )

  header = file_lines[header_line] %>% stringr::str_split("\\s*,\\s*", simplify = T)
  df = purrr::map_dfr(file_lines[data_start_line:data_end_line], function(x) {
    stringr::str_split(x, "\\s*,\\s*", simplify = T) %>%
      as.data.frame(stringsAsFactors = F)
  }) %>% setNames(header)
  print("end_func::parse_file")
  return(df)
}

infer_date_time = function(column) {
  print("func::infer_date_time")
  lubridate::parse_date_time(column, orders = c("mdY T", "dmY T", "Ymd T", "Ydm T"))
}

infer_light = function(df, start, end) {
  print("func::infer_light")
  start = lubridate::hms(start)
  end = lubridate::hms(end)
  df$date_time = infer_date_time(df$date_time)
  df = df %>%
    dplyr::mutate(light = ifelse(hms::as_hms(date_time) >= start & hms::as_hms(date_time) < end, 1, 0))
  print("end_func::infer_light")
  return(df)
}

prettify_data = function(df, col_specs, type) {
  print("func::prettify_data")
  df = df %>%
    dplyr::select(col_specs$colname[!is.na(col_specs$colname)]) %>%
    setNames(col_specs %>%
      dplyr::select(colname, app) %>%
      tidyr::drop_na() %>%
      dplyr::pull(app))

  df = purrr::map_dfc(df, ~ type.convert(., as.is = TRUE))
  df = df %>%
    mutate(subject = as.character(subject))

  # TODO fix the subject issue better
  if (type == "clams") {
    df = df %>%
      dplyr::mutate(
        xyt = xt + yt,
        xf = xt - xa,
        yf = yt - ya
      ) %>%
      dplyr::mutate(light = dplyr::case_when(
        light == "ON" ~ 1,
        light == "OFF" ~ 0,
        TRUE ~ 0
      )) %>%
      dplyr::mutate(subject = dplyr::pull(., subject)[1])
  }

  if (type == "tse") {
    df = df %>%
      dplyr::mutate(date_time = paste0(Date, " ", Time)) %>%
      dplyr::mutate(light = dplyr::case_when(
        light > 30 ~ 1,
        TRUE ~ 0
      ))
  }

  df$date_time = infer_date_time(df$date_time)
  df = df %>%
    dplyr::select(col_specs$app[!is.na(col_specs$app)])
  print("end_func::prettify_data")
  return(as.data.frame(df))
}

### processing of parameter selection ------
validate_param_df = function(parameter_df) {
  print("func::validate_param_df")
  if (anyDuplicated(parameter_df$select) > 0) {
    print("Duplicated parameters selected.")
    return(FALSE)
  } else if (anyDuplicated(parameter_df$select) > 0) {
    print("Duplicated parameter display names.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}

### find measurement frequences in the data ------
find_frequencies = function(df, subject_col, date_time_col) {
  print("func::find_frequencies")
  date_time_df = df %>%
    dplyr::group_by({{ subject_col }}) %>%
    dplyr::mutate(frequency = dplyr::row_number()) %>%
    tidyr::pivot_wider(
      names_from = {{ subject_col }},
      values_from = {{ date_time_col }},
      id_cols = frequency
    )
  frequencies = purrr::map_dfr(date_time_df[-1], function(x) {
    diff(x) %>% as.integer()
  }) %>%
    tidyr::pivot_longer(everything(),
      names_to = "subject",
      values_to = "frequency"
    ) %>%
    dplyr::pull(frequency) %>%
    unique()
  print("end_func::find_frequencies")
  return(frequencies)
}

### time aggregation of data ------
recursive_divisors = function(v) {
  print(v)
  if (length(v) == 1) {
    return(numbers::divisors(v))
  }
  else if (length(v) == 2) {
    return(intersect(numbers::divisors(v[2]), numbers::divisors(v[1])))
  } else {
    return(intersect(rec_intersect(v[2:length(v)]), numbers::divisors(v[1])))
  }
}

get_valid_time_agg = function(freq, phase_durations) {
  print("func::get_valid_time_agg")
  phase_intersect = recursive_divisors(phase_durations)
  values = intersect(
    seq(freq, 24 * 60, by = freq),
    c(phase_intersect, 1440)
  )
  repeats = values / freq
  print("end_func::get_valid_time_agg")
  return(list("values" = values, "repeats" = repeats))
}

create_aggregation_vector = function(each, length) {
  print("func::create_aggregation_vector")
  vec = c(rep(1:length, each = each, length.out = length))
  print("end_func::create_aggregation_vector")
  return(vec)
}

create_aggregation_df = function(phase_durations, frequency, l) {
  print("func::create_aggregation_df")
  time_agg = get_valid_time_agg(frequency, phase_durations)
  v = purrr::map_dfc(time_agg$repeats,
    .f = create_aggregation_vector,
    l
  ) %>%
    setNames(paste0("t", time_agg$values))
  print("end_func::create_aggregation_df")
  return(v)
}

aggregate_with_specs = function(param_df) {
  function(select_param) {
    by = param_df %>% dplyr::filter(app == select_param) %>% dplyr::select(aggregate) %>% as.character
    if(by == "first"){
      return(getFromNamespace("first", "dplyr"))
    } else {
      return(get(by))
    }
  }
}

aggregate_parameter = function(data, time, param, by) {
  print("func::aggregate_parameter")
  data = data.table::setDT(data)[, .(
    light = data.table::first(light),
    period = data.table::first(period),
    date_time = data.table::first(date_time),
    mean = by(get(param)),
    param = param
  ),
  by = .(subject, interval = get(time))
  ]
  data[, `:=`(cumsum = cumsum(mean)), by = .(subject, param)]
  print("end_func::aggregate_parameter")
  return(data)
}

aggregate_selected_params = function(data, time, params, param_df) {
  print("func::aggregate_selected_params")

  tdf = purrr::map_dfr(
    params,
    ~ aggregate_parameter(
      data,
      paste0("t", time),
      .x,
      aggregate_with_specs(param_df)(.x)
    )
  )
  tdf$param = factor(tdf$param, levels = unique(tdf$param))
  print("end_func::aggregate_selected_params")
  return(tdf)
}

# input based data filtering functions ------
filter_subjects = function(df, subjects) {
  print("func::filter_subjects")
  df %>%
    dplyr::filter(subject %in% subjects)
}

filter_intervals = function(df, minval, maxval) {
  print("func::filter_intervals")
  df %>%
    dplyr::filter(interval >= minval) %>%
    dplyr::filter(interval <= maxval)
}

filter_periods = function(df, periods) {
  print("func::filter_periods")
  if(is.null(periods)) {
    d = data.frame(matrix(nrow=0, ncol = length(names(df)))) %>%
      setNames(names(df))
    print(d)
    return(d)
  } else {
    df %>% dplyr::filter(period %in% periods)
  }

}


# group related functions ------
parse_group_inputs = function(input, counter) {
  print("func::parse_group_inputs")
  group_list = purrr::map(1:counter, function(x) {
    input[[paste0("group_no_", x)]]
  })

  group_list = group_list %>% purrr::discard(is.null)

  if (length(group_list) > 0) {
    group_df = purrr::map_dfr(1:length(group_list), function(x) {
      cbind.data.frame(
        subject = group_list[[x]],
        group = input[[paste0("group_name_", x)]],
        stringsAsFactors = FALSE
      ) %>%
        dplyr::mutate(group = ifelse(group == "", paste0("Group", x), group))
    })
  } else {
    group_df = data.frame(subject = character(0),
                          group = character(0),
                          stringsAsFactors = FALSE)
  }
  print("end_func::parse_group_inputs")
  print(group_df)
  return(group_df)
}

parse_group_file = function(input) {
  group_df = readr::read_csv(input$metadata$datapath,
                             col_types = readr::cols(.default = "c"))
  group_df = group_df %>%
    dplyr::select(subject, group = matches(paste(input$metadata_group_col)))
  print(group_df)
  return(group_df)
}

add_groups = function(data_df, group_df) {
  print("func::add_groups")
  if (dim(group_df)[1] == 0) {
    group_df = data_df %>%
      dplyr::select(subject) %>%
      unique() %>%
      dplyr::mutate(
        group = subject
      )
  }
  df = data_df %>%
    dplyr::left_join(group_df, by = "subject") %>%
    dplyr::mutate(group = as.character(group))
  print("end_func::add_groups")
  return(df)
}

summarise_groups_bool = function(df, grouped, filter = F, subjects = NULL) {
  print("func::summarise_groups_bool")
  if(grouped == "group") {
    df %>% summarise_groups()
  } else {
    if(filter == T) {
      df %>% filter_subjects(subjects)
    } else {
      df
    }
  }
}

summarise_groups = function(df) {
  print("func::summarise_groups")
  df = df %>%
    dplyr::group_by(across(c(-subject, -date_time, -mean))) %>%
    dplyr::summarise(sd = sd(mean, na.rm = T),
                     mean = mean(mean, na.rm = T),
                     .groups = "drop") %>%
    dplyr::group_by(group, param) %>%
    dplyr::mutate(cumsum = cumsum(mean))
  print("end_func::summarise_groups")
  return(df)
}



get_dark_interval_ycoord = function(data, plot_y) {
  if(!("sd" %in% names(data))) {
    data$sd = 0
  }
  data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sd_mean_max = sum(!!as.symbol(plot_y), sd, na.rm = T),
                  sd_mean_min = sum(!!as.symbol(plot_y), -1*sd, na.rm = T)) %>%
    dplyr::group_by(param) %>%
    dplyr::summarise(
      ymin = min(sd_mean_min),
      ymax = max(sd_mean_max),
      .groups = "drop") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    ymin = ymin - 0.03 * (ymax - ymin),
    ymax = ymax + 0.03 * (ymax - ymin)
  )
}

get_dark_interval_rect = function(data, grp, plot_y, shift_by = 0) {
  print("func::get_dark_interval_rect")

  min_interval = min(data$interval)
  max_interval = max(data$interval)

  df = data %>%
    filter(!!as.symbol(grp) == dplyr::first(!!as.symbol(grp))) %>%
    dplyr::group_by(light, period, param) %>%
    summarise(lengths = rle(light)$lengths, .groups = "drop") %>%
    arrange(param, period) %>%
    group_by(param) %>%
    mutate(xmax = (cumsum(lengths) + shift_by) + min_interval) %>%
    mutate(xmin = (xmax - lengths)) %>%
    dplyr::left_join(get_dark_interval_ycoord(data, plot_y), by = "param") %>%
    dplyr::filter(light == 0) %>%
    dplyr::filter(xmax > min_interval,
                  xmin <= max_interval)
  return(df)
}

get_circadian_dark_interval_rect = function(data, grp, plot_y, shift_by = 0) {
  print("func::get_circadian_dark_interval_rect")
  df = data %>%
    filter(!!as.symbol(grp) == dplyr::first(!!as.symbol(grp))) %>%
    dplyr::group_by(light, param) %>%
    summarise(lengths = rle(light)$lengths, .groups = "drop") %>%
    arrange(param) %>%
    group_by(param) %>%
    mutate(xmax = (cumsum(lengths) + shift_by)) %>%
    mutate(xmin = (xmax - lengths)) %>%
    dplyr::left_join(get_dark_interval_ycoord(data, plot_y), by = "param") %>%
    dplyr::filter(light == 0)
  return(df)
}

plot_activity_boxjitter = function(data, plot_x, plot_y, time_agg, ...) {
  data %>%
    #ungroup() %>%
    mutate(shift_jitter = as.numeric(as.factor(!!as.symbol(plot_x))) - 0.35) %>%
    ggplot() +
    geom_boxplot(aes(x = !!as.symbol(plot_x),
                     y = !!as.symbol(plot_y) * as.numeric(time_agg),
                     fill = !!as.symbol(plot_x)),
                 outlier.shape = NA,
                 size = 0.5) +
    geom_jitter(aes(x = shift_jitter,
                    y = !!as.symbol(plot_y) * as.numeric(time_agg),
                    colour = !!as.symbol(plot_x),
                    text = !!as.symbol(plot_y) * as.numeric(time_agg)),
                alpha = 0.7,
                width = 0.05) +
    facet_wrap(~ activity_value, nrow = 1) +
    theme_minimal() +
    theme(legend.position = "none")
}

plot_points = function(condition_field, aes_colour) {
  if ("1" %in% condition_field) {
    geom_point(aes_string(color = aes_colour), size = 0.75)
  } else {
    geom_blank()
  }
}

plot_errorbars = function(condition_field, aes_fill) {
  if ("1" %in% condition_field) {
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = !!as.symbol(aes_fill)), alpha = 0.08)
  } else {
    geom_blank()
  }
}

plot_facets = function(data, formula = "param ~ .") {
  if (length(data$param %>% unique()) > 1) {
    facet_grid(as.formula(formula), scales = "free_y", labeller = label_both)
  } else {
    geom_blank()
  }
}

# TODO test for multiple subjects
plot_lm = function(condition_field) {
  if ("1" %in% condition_field) {
    geom_smooth(method = 'lm')
  } else {
    geom_blank()
  }
}

plot_dark_interval_rect = function(data) {
  if(dim(data)[1] > 0) {
    geom_rect(
      data = data,
      mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "grey70",
      alpha = 0.2,
      inherit.aes = F
    )
  } else {
    geom_blank()
  }
}

calculate_correlations = function(data, grp, x, y) {
  print("func::calculate_correlations")
  data %>%
    dplyr::group_by(!!as.symbol(grp), light) %>%
    dplyr::summarise(cor.test(!!as.symbol(x), !!as.symbol(y)) %>% broom::tidy(), .groups = "drop")
}

calculate_pvals = function(df, grouping_cols) {
  print("func::calculate_pvals")
  if(length(df$group %>% unique) > 1) {
    df = df %>%
      data.frame(stringsAsFactors = F) %>%
      tidyr::nest(nested = !{{grouping_cols}}) %>%
      dplyr::mutate(stat = purrr::map(nested, function(x) {
        pairwise.t.test(x$mean, x$group, p.adjust.method = "none") # %>%
          broom::tidy()
      })) %>%
  tidyr::unnest(cols = stat) %>%
  dplyr::mutate(padj = p.adjust(p.value, method = "BH")) %>%
  dplyr::select(param, light, group1, group2, pval = p.value, padj)
  } else {
    data.frame()
  }
}

calculate_activity_summary = function(data, grp) {
  print("func::calculate_activity_summary")
  activity_summary = data %>%
    dplyr::group_by(!!as.symbol(grp), param) %>%
    dplyr::summarise(
      activity_length = rle(activity)$lengths,
      activity_value = rle(activity)$values,
      .groups = "drop"
    ) %>%
    dplyr::mutate(id = dplyr::row_number())

  activity_totals = data %>%
    dplyr::mutate(id = rep(activity_summary$id, activity_summary$activity_length)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(sum = sum(mean), .groups = "drop")

  activity_summary = activity_summary %>%
    dplyr::left_join(activity_totals, by = "id")
  return(activity_summary)
}
