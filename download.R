output$download <- renderUI({
  default_aggregations = c(10, 30, 60, 180, 720)
  
  fluidRow(
    map(1:5, function(x) {
      selectInput(paste0("select_aggregation_",x), paste0(label = "Select aggregation ", x, " [min]"), 
                  choices = time_aggregation_values, 
                  selected = default_aggregations[x])
    }),
    actionButton("download_xlsx", label = "Download", icon = icon("download"))
  )
})


# output$download_xlsx <- observeEvent({
#   
# })