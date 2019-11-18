observeEvent(input$add_btn, {
  if(counter$n < 6) counter$n <- counter$n + 1
})

observeEvent(input$rm_btn, {
  if(counter$n > 1) counter$n <- counter$n - 1
})

parameter_boxes = reactive({
  n <- counter$n
  if (n >= 1) {
    lapply(seq_len(n), function(i) {
      selectInput(paste0("select_parameter_",i), label = NULL,
                  choices = global_vars$parameters,
                  selected =  input[[paste0("select_parameter_", i)]])
    })
  }
})

output$textbox_ui <- renderUI({ parameter_boxes() })