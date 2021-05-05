##### LEC match server #####

# Display table
output$LEC_matches <- DT::renderDataTable({
  res_table <- buildTable(data = data_LEC, number = input$LEC_Match_nb)
  
  DT::datatable(
    res_table[[1]],
    options = list(dom = 't',
                   scrollX = TRUE,
                   ordering = FALSE,
                   columnDefs = list(list(className = 'dt-center', targets = "_all"))),
    rownames = FALSE,
    caption =  htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center;',
      htmltools::strong(res_table[[2]])
    )
  )
})
