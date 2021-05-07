readInput = observe({

  file <- input$file1
  ext <- tools::file_ext(file)[1]

  # if file is not uploaded provide temp file to show results
  if(is.null(file)) {
  file <- read_delim("2019-12-04_classic.csv", delim = ',',col_types = "ciciddddiiiiiiiidd")
  } else if(toupper(ext) == "CSV" | toupper(ext) == "TXT") {
    file.rename(file$datapath,
                paste(file$datapath, ext, sep="."))
    file <- read_delim(paste(file$datapath, ext, sep="."), delim = ',',col_types = "cicdiddddiiiiiiiiddc")
  }

  data = file
  subject_list = unique(data$subject)
  
  parameters = column_specs %>% dplyr::filter(parameter == 1) %>% select(name_app) %>% pull
  names(parameters) = column_specs %>% filter(parameter == 1) %>% select(display_app) %>% pull
  
  interval = find_interval(data, subject, date_time, interval)
  print(interval)
  #interval = 2
  if(length(interval) != 1) {
    stop("One of subject time series is not regular. Please update your data and try again")
  } else {
    interval = as.numeric(interval)
  }
  rv_data$interval = interval
  
  time_aggregation_values = intersect(seq(interval, 24*60, by = interval), 
                                      c(divisors(12*60)[-1], 1440))
  time_aggregation_repeats = time_aggregation_values / interval
  
  data_subject = data %>% dplyr::group_by(subject) %>% nest()
  data_subject = data_subject %>% 
    mutate(first_night_interval = map(data, . %>% dplyr::filter(light == 0) %>% top_n(1, -interval) %>% dplyr::select(interval) %>% as.numeric),
           no_records = map(data, . %>% dplyr::count() %>% as.numeric),
           cropped_records = map2_dbl(.x = data, .y = first_night_interval, function(x, y) {(x %>% dplyr::count() %>% as.numeric) + 1 - y}))
  
  min_records = min(data_subject$cropped_records)
  
  data_subject = data_subject %>%
    mutate(cropped = modify2(data, first_night_interval, function(x, y, mm) {
      x %>% dplyr::filter(interval >= y & interval <= (mm + y))
    }, mm = min_records))
  
  aggdf = map_dfc(time_aggregation_repeats, .f = create_aggregation_vector, data_subject$cropped_records[[1]])
  names(aggdf) = paste0("t",time_aggregation_values)
  
  data_long = data_subject %>% select(subject, cropped) %>% unnest(cropped)
  data_agg = cbind.data.frame(data_long, aggdf)
  
  rv_data$data_subject = data_subject
  rv_data$data_agg = data_agg
  rv_data$column_specs = column_specs
  rv_data$parameters = parameters
  rv_data$subject_list = subject_list
  rv_data$time_aggregation_values = time_aggregation_values
  rv_data$time_aggregation_repeats = time_aggregation_repeats
})