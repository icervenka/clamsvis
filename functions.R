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



aggregate_parameter = function(data, time, param) {
  func = aggregate_by(param)
  data = setDT(data)[,.(light = data.table::first(light),
                 date_time = data.table::first(date_time),
                 mean = func(get(param)),
                 param = param),
              by = .(subject, interval = get(time))]
  data[,period := cumsum(c(1,diff(light)!=0)), by = subject]
  return(data)
}

parse_group_inputs = function(inp) {
  no_groups = 1
  if(!is.null(inp$select_no_groups)) {
    no_groups = inp$select_no_groups
  }

  group_list = lapply(1:as.integer(no_groups), function(x) {
    inp[[paste0("group_no_", x)]]
  })

  group_list[sapply(group_list, is.null)] <- list("")
  group_list <- group_list[group_list != ""]

  group_list <- rapply(lapply(group_list, strsplit, ","), str_trim, how = "list") %>%
    lapply(unlist)

  if(length(group_list) > 0) {
    group_df = map_dfr(1:length(group_list), function(x) {
      cbind.data.frame(subject = group_list[[x]], group =  paste0("Group", x), stringsAsFactors = FALSE)
    })
  } else {
    group_df = data.frame()
  }

  return(group_df)
}

# min.mean.sd.max <- function(x) {
#   r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
#   names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
#   r
# }
#
# mean.sd <- function(x) {
#   r <- c(mean(x) - sd(x), mean(x), mean(x) + sd(x))
#   names(r) <- c("ymin", "y", "ymax")
#   r
# }


aggregate_with_specs = function(specs) {
  function(select_param) {
    by = specs %>% dplyr::filter(name_app == select_param) %>% dplyr::select(aggregate) %>% as.character
    return(get(by))
  }
}


# additional ploting options ---------------------------------
# TODO add geom_tile

plot_points = function(condition_field, aes_colour = subject) {
  if("1" %in% condition_field) {
    geom_point(aes(colour = {{ aes_colour }} ), size = 0.75)
  } else {
    geom_blank()
  }
}

plot_errorbars = function(condition_field, aes_fill = group) {
  if(condition_field == "2") {
    geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd, fill = {{ aes_fill }}), alpha = 0.08)
  } else {
    geom_blank()
  }
}

plot_facets = function(n, formula = "param ~ ." ) {
  if(n > 1) {
    facet_grid(as.formula(formula) ,scales = "free_y", labeller = label_both)
  } else {
    geom_blank()
  }
}

# plot_jitter = function(condition_field) {
#   if("1" %in% condition_field) {
#     geom_jitter(shape = 21, colour = "grey50", fill = "white", size = 1, stroke = 0.25, width = 0.25)
#   } else {
#     geom_blank()
#   }
# }
