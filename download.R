output$download <- renderUI({
  
  default_aggregations = c(10, 30, 60, 180, 720)
  
  fluidRow(
    map(1:5, function(x) {
      selectInput(paste0("select_aggregation_",x), paste0(label = "Select aggregation ", x, " [min]"), 
                  choices = rv_data$time_aggregation_values, 
                  selected = default_aggregations[x])
    }),
    downloadButton("download_xlsx", label = "Download", icon = icon("download"))
  )
})

output$download_xlsx <- downloadHandler(
  
  filename = function() {
    paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_clams_vis", ".xlsx")
  },
  
  content = function(file) {
    #tagg = c(10, 30, 60, 180, 720)
    
    tagg = map(1:5, function(x) {
      input[[paste0("select_aggregation_",x)]]
    }) %>% as.vector(mode = "integer")
    
    data_long_agg = map_dfr(tagg, function(x) {
      df = map_aggregate(rv_data$parameters, rv_data$data_agg, x)
      cbind.data.frame(df, aggregation = x, stringsAsFactors = FALSE)
    })

    wb<-createWorkbook(type="xlsx")
  
    walk(rv_data$parameters, function(p, datap) {
      sheet <- createSheet(wb, sheetName = p)
      dfp = datap %>% filter(param == p)
      
      walk(seq_along(tagg), function(a, dataa) {
        dfa = dataa %>% 
          dplyr::filter(aggregation == tagg[a]) %>% 
          pivot_wider(id_cols = c(interval, date_time, light), names_from = subject, values_from = mean)

        addDataFrame(as.data.frame(dfa), sheet, startRow=1, startColumn=(dim(dfa)[2]+1)*(a-1) + 1, row.names=FALSE)
  
      }, dataa = dfp)
    }, datap = data_long_agg)
    saveWorkbook(wb, file)
  }
)