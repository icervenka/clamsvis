# functions -----------------------------------------------
find_interval = function(df, group_col, date_time_col, id_col) {
  date_time_df = df %>% pivot_wider(names_from = {{group_col}}, values_from = {{date_time_col}}, id_cols = {{id_col}})
  interval = map_dfr(date_time_df[-1], function(x) {diff(x %>% as_datetime) %>% as.integer}) #%>%
  #pivot_longer(everything(), names_to = "subject", values_to = "interval") %>%
  #dplyr::select(interval) #%>%
  # unique %>%
  # pull
  return(interval)
}

create_aggregation_vector = function(each, length) {
  vec = c(rep(1:length, each = each, length.out = length))
  return(vec)
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
