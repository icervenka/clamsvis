# imports ------------------------------------------------
library(shiny)
library(dplyr)
library(stringr)
library(readr)
library(xlsx)
library(ggplot2)
library(readr)
library(purrr)
library(tidyverse)
library(numbers)
library(rJava)
library(reshape2)
library(matrixStats)

# options ------------------------------------------------
options(shiny.maxRequestSize = 20*1024^2)
options(java.parameters = "-Xmx2048m")

# functions -----------------------------------------------
# create_aggregation_vector = function(length, each) {
#   # vec = map_dfc(repeats, function(x) {
#   #   c(rep(NA, skip_start-1), rep(1:length, each = x, length.out = length+1))
#   # })
#   vec = c(rep(1:length, each = each, length.out = length))
#   return(vec)
# }

create_aggregation_vector = function(each, length) {
  # vec = map_dfc(repeats, function(x) {
  #   c(rep(NA, skip_start-1), rep(1:length, each = x, length.out = length+1))
  # })
  vec = c(rep(1:length, each = each, length.out = length))
  return(vec)
}

aggregate_parameter = function(data, subject, aggregation, parameter = "vo2", time = "t360", by = "mean", cumulative = FALSE) {
  tmp = aggregate(data %>% dplyr::select(parameter), aggregation %>% dplyr::select(time), by)
  if(cumulative == TRUE) {
    tmp = cbind.data.frame(tmp %>% dplyr::select(1), tmp %>% dplyr::select(-1) %>% cumsum)
  }
  return(cbind.data.frame(tmp, subject, stringsAsFactors = F))
}

group_aggregate_paramter = function(data, aggregation, parameter = "vo2", time = "t360", by = mean, cumulative = FALSE) {
  tmp = data %>% select(subject, cropped) %>% unnest(cropped) %>% select(subject, interval, light, parameter)
  cast_tmp = dcast(tmp, interval + light ~ subject, value.var = parameter)
  agg_tmp = aggregate(cast_tmp[,-c(1,2)], aggregation %>% select(time), by)
  return(agg_tmp)
}

group_means = function(aggregated_df, groups) {
  group_tmp = map_dfr(groups, function(x) {
    tmp_mean = aggregated_df %>% select(x) %>% rowMeans
    tmp_sd = aggregated_df %>% select(x) %>% as.matrix %>% rowSds
    grp_name = paste(x, collapse = '|')
    cbind.data.frame(interval = aggregated_df[,1], mean = tmp_mean, sd = tmp_sd, group = grp_name, stringsAsFactors = F)
  })
}

aggregate_by = function(parameter) {
  by = column_specs %>% dplyr::filter(name_app == select_param) %>% dplyr::select(aggregate) %>% as.character
  return(by)
}

# constants -----------------------------------------------
#TODO verify that all intervals are the same
# will inferred from data
interval = 2

# will be selectable
select_param = "rer"
select_agg = "t360"

fasting <- "Fasting"
refeeding <- "Refeeding"

# will be selectable
subject_list = c("1001", "1002", "1003", "1004", "1005", "1003", "1016")
groups = list(c("1001", "1002"), c("1003", "1004"), c("1005", "1006"))

# load data -----------------------------------------------
#setwd("E:/OneDrive/programming/clams/")
setwd("~/OneDrive/programming/clams/")

column_specs = read_delim("clams_column_specification.txt", delim = '\t')

# -1 -1  is because temp and light is still present in this iteration
#parameters = column_specs$name_app[!is.na(column_specs$aggregate)][-1][-1]

parameters = column_specs %>% filter(!is.na(aggregate)) %>% select(name_app) %>% pull
names(parameters) = column_specs %>% filter(!is.na(aggregate)) %>% select(display_app) %>% pull

parameters = parameters[-1][-1]

data = read_delim("2019-10-16_tse.csv", delim = ',',
                  col_types = "cicdiddddiiiiiiiiddc")
data$temp = NULL
data$events = NULL

#colpal = c("#F8766D", "#E68613", "#CD9600", "#ABA300", "#7CAE00", "#0CB702", "#00BE67", "#00C19A", "#00BFC4", "#00B8E7", "#00A9FF", "#8494FF", "#C77CFF", "#ED68ED","#FF61CC", "#FF68A1")

# analysis ------------------------------------------------
time_aggregation_values = intersect(seq(interval, 24*60, by = interval), 
                                    c(divisors(12*60)[-1], 1440))
time_aggregation_repeats = time_aggregation_values / interval

data_nest = data %>% group_by(subject) %>% nest()
data_nest = data_nest %>% 
  mutate(first_night_interval = map(data, . %>% dplyr::filter(light == 0) %>% top_n(1, -interval) %>% dplyr::select(interval) %>% as.numeric),
         no_records = map(data, . %>% count() %>% as.numeric),
         cropped_records = map2_dbl(.x = data, .y = first_night_interval, function(x, y) {(x %>% count() %>% as.numeric) + 1 - y}))



#min records not implemented
#min_records = min(map2_dbl(data_nest$cropped_records, data_nest$first_night_interval, `-`))
min_records = min(data_nest$cropped_records)

data_nest = data_nest %>%
  mutate(cropped = modify2(data, first_night_interval, function(x, y, mm) {
    x %>% dplyr::filter(interval >= y & interval <= (mm + y))
  }, mm = min_records))

# data_nest = data_nest %>%
#   mutate(aggdf = map2(no_records, first_night_interval, .f = create_aggregation_vector, time_aggregation_repeats))

# data_nest = data_nest %>%
#   mutate(aggdf = list(map2_dfc(cropped_records, time_aggregation_repeats, .f = create_aggregation_vector)))

aggdf = map_dfc(time_aggregation_repeats, .f = create_aggregation_vector, data_nest$cropped_records[[1]])
names(aggdf) = paste0("t",time_aggregation_values)

# param_time_grid = expand.grid(param = parameters, time = names(aggdf), stringsAsFactors = F)
# 
# all_agg_param = map_dfr(data_nest$cropped, function(x, ptg, aggdf) {
#   map2(ptg$param, ptg$time, function(y, z, aggdf) {
#     aggregate(x %>% select(y), aggdf %>% select(z), mean)
#     #names(pt_agg) = c("param", "time")
#   }, aggdf = aggdf)
# }, ptg = param_time_grid, aggdf = aggdf)
# 
# 
# tt = map2_dfr(param_time_grid, function(y, aggdf, x) {
#   print(y)
#   #print(y)
#   #aggregate(x %>% select(y[[1]]), aggdf %>% select(y[[2]]), mean)
# }, aggdf = aggdf, x = data_nest$cropped[[1]])

  
# rename columns off aggregation data frame to form t(minutes) to work with select input
# data_nest$aggdf = data_nest$aggdf %>% map(`names<-`, paste0("t",time_aggregation_values))

# calculate aggregated data frame for selected parameter and time
# see if I can't pass a subsetted data frame instead of specifying the columns
# aggregated_df = map2_dfr(data_nest$cropped, data_nest$subject,
#                   .f = aggregate_parameter, aggdf, select_param, select_agg, aggregate_by(select_param))
# 
# # rename columns so all the aggregations have the same signature
# colnames(aggregated_df) = c("interval", "parameter", "subject")
# 
# #plot for individuals
# aggregated_df %>%
#   #dplyr::filter(subject %in% subject_list) %>%
#   ggplot(aes(x = interval, y = parameter, color = subject)) + geom_line()
# 
# 
# s1 = c("1001", "1002")
# s2 = c("1003", "1004")
# s3 = c("1005", "1006")
# 
# groups = list(c("1001", "1002"), c("1003", "1004"), c("1005", "1006"))
# 
# tt = data_nest %>% select(subject, cropped) %>% unnest(cropped) %>% select(subject, interval, light, vo2)
# dt = dcast(tt, interval + light ~ subject, value.var = "vo2")
# df = aggregate(dt[,-c(1,2)], aggdf %>% select(t360), mean)
# 
# fd = map_dfr(groups, function(x) {
#   tmp_mean = df %>% select(x) %>% rowMeans
#   tmp_sd = df %>% select(x) %>% as.matrix %>% rowSds
#   grp_name = paste(x, collapse = '|')
#   cbind.data.frame(interval = df[,1], mean = tmp_mean, sd = tmp_sd, group = grp_name, stringsAsFactors = F)
#   #names(tmp_agg) = c("a", "b")
# })
# 
# 
# 
# 
# 
# 
# data_cropped = data_nest %>% dplyr::select(subject, cropped) %>% unnest(c(cropped))
# data_cropped_melt = melt(data_cropped, id.vars = c("interval", "date_time", "light", "subject"), measure.vars = c(6:19))
# parameter_nest = data_cropped_melt %>% group_by(variable) %>% nest()
# parameter_nest_melt = parameter_nest %>% dplyr::mutate(melted = map(data, dcast, formula = interval + date_time + light ~ subject, value.var = "value"))
# 
# 
# 
# parameter_nest_melt = parameter_nest_melt %>% mutate(aggdf = list(data_nest$aggdf[[1]]))
# parameter_nest_melt = parameter_nest_melt %>%
#   dplyr::mutate(no_records = map(melted, . %>% count() %>% as.numeric))
# 
# parameter_nest_melt = parameter_nest_melt %>%
#   mutate(aggdf = list(map2_dfc(no_records, time_aggregation_repeats, .f = create_aggregation_vector)))
# parameter_nest_melt$aggdf = parameter_nest_melt$aggdf %>% map(`names<-`, paste0("t",time_aggregation_values))
# 
# 
# aggregate_parameter_2 = function(data, aggregation, subject_list) {
#   tmp = aggregate(data %>% dplyr::select(subject_list), aggregation %>% dplyr::select("t60"), mean)
#   ind = aggregate(data %>% dplyr::select(date_time, light), aggregation %>% dplyr::select("t60"), first)
#   return(cbind.data.frame(ind, tmp))
# }
# 
# d = parameter_nest_melt %>% select(variable, melted) %>% filter(variable == "vo2") %>%
#   unnest(cols = melted) %>% ungroup %>% select(-variable)
# agg = parameter_nest_melt %>% select(variable, aggdf) %>% filter(variable == "vo2") %>%
#   unnest(cols = aggdf) %>% ungroup %>% select(-variable)
# 
# map(names(agg), function(x, data) {
#   aggregate(data[,5:10], agg %>% select(x), mean)
# }, data = d)
# 
# aggregate_parameter_3 = function(parameter, data, specs) {
#   d = data %>% select(variable, melted) %>% filter(variable == parameter) %>%
#     unnest(cols = melted) %>% ungroup %>% select(-variable)
#   agg = data %>% select(variable, aggdf) %>% filter(variable == parameter) %>%
#     unnest(cols = aggdf) %>% ungroup %>% select(-variable)
#   
#   
#   wb<-createWorkbook(type="xlsx")
#   
#   map(names(agg), function(x, par_data, by) {
#     ind = aggregate(par_data %>% dplyr::select(date_time, light), agg %>% select(x), first)
#     res = aggregate(par_data[,data_nest$subject], agg %>% dplyr::select(x), by)
#     save_df = cbind.data.frame(ind, res[,-1])
#     sheet <- createSheet(wb, sheetName = x)
#     addDataFrame(save_df, sheet, startRow=1, startColumn=1, row.names=FALSE)
#     return(save_df)
#   }, par_data = d, by = specs %>% filter(name_app == parameter) %>% select(aggregate) %>% as.character())
#   
#   saveWorkbook(wb, paste0(parameter,".xlsx"))
#   
# }
# 
# 
# 
# map(parameter_nest_melt$variable, aggregate_parameter_3, parameter_nest_melt, column_specs)
# 
# 
# 
# select_parameter = parameter_nest_melt %>% filter(variable == "vo2")
# parameter_df = map2_df(select_parameter, select_agg, .f= aggregate_parameter_2)
# 
# 
# 
# export_clams_by_parameter(nested_data, parameter, time_aggregations = c(720, 1440)) {
#   time_agg_columns = paste0("t", time_aggregations)
#   
#   res = nested_data
#   
#   result = pmap_dfr(list(nested$data, data_nest$aggdf, data_nest$subject), 
#                            .f = aggregate_parameter, select_param, select_agg, aggregate_by(select_param))
#   
#   result_cast = dcast(aggregated_df, interval ~ subject, value.var = "parameter")
#   
# }
# 
# tmp_melt = melt(data, id.vars = c("interval", "date_time", "light", "subject"), measure.vars = c(6:19))
# parameter_nest = tmp_melt %>% group_by(variable) %>% nest()
# parameter_nest_melt = parameter_nest %>% dplyr::mutate(melt = map(data, dcast, formula = interval + date_time + light ~ subject, value.var = "value"))
