output$download <- renderUI({
  
  default_aggregations = c(10, 30, 60, 180, 720)
  
  fluidRow(
    map(1:5, function(x) {
      selectInput(paste0("select_aggregation_",x), paste0(label = "Select aggregation ", x, " [min]"), 
                  choices = global_vars$time_aggregation_values, 
                  selected = default_aggregations[x])
    }),
    downloadButton("download_xlsx", label = "Download", icon = icon("download"))
  )
})

output$download_xlsx <- downloadHandler(
  
  filename = function() {
    paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_clams", ".xlsx")
  },
  
  content = function(file) {
    tagg = c("t10", "t30", "t60", "t180", "t720")
  
    data_parameter = map_dfr(parameters, function(x, data) {
      df = dcast(data %>% select(subject, interval, date_time, light, x), interval + date_time + light ~ subject, value.var = x)
      cbind.data.frame(df, parameter = x, stringsAsFactors = FALSE)
    }, global_vars$data_subject %>% select(subject, cropped) %>% unnest(cropped))
  
    data_parameter = data_parameter %>% group_by(parameter) %>% nest()
  
    wb<-createWorkbook(type="xlsx")
  
    walk(parameters, function(p) {
      sheet <- createSheet(wb, sheetName = p)
      walk2(tagg, seq_along(tagg), function(x, y, data, parameter, aggregation) {
  
        parameter_df = data %>% filter(parameter == p) %>% `[[`(1,2)
  
        tt = aggregate(parameter_df %>% dplyr::select(date_time, light), aggregation %>% dplyr::select(x), first)
        dt = aggregate(parameter_df %>% dplyr::select(-interval, -date_time, -light), aggregation %>% dplyr::select(x), aggregate_by(p))
        mt = merge(tt, dt)
  
        addDataFrame(mt, sheet, startRow=1, startColumn=(dim(mt)[2]+1)*(y-1) + 1, row.names=FALSE)
  
      }, data = data_parameter, parameter = p, aggregation = aggdf)
    })
    saveWorkbook(wb, file)
  }
)