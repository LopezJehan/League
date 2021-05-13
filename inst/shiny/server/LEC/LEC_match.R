##### LEC match server #####
blue_teams_LEC <- data_LEC %>% 
  filter(position == "team",
         side == "Blue") %>% 
  select(team, result)

red_teams_LEC <- data_LEC %>% 
  filter(position == "team",
         side == "Red") %>%
  select(team, result) 

matchs_LEC <- data.frame(blue_teams_LEC, red_teams_LEC) %>% 
  rename("Blue" = team,
         "Red" = team.1,
         "Blue_result" = result,
         "Red_result" = result.1) %>% 
  mutate(row = 1:nrow(matchs_LEC),
         Result = paste(row, "-", Blue, Blue_result, "-", Red_result, Red))

output$LEC_Match_picker <- renderUI(
  pickerInput("LEC_match_picker",
              "Choose a match",
              choices = matchs_LEC$Result,
              selected = NULL,
              multiple = TRUE,
              options = pickerOptions(maxOptions = 1))
)

# Display table
output$LEC_matches <- DT::renderDataTable({
  if(!is.null(input$LEC_match_picker)){
    res_table <- buildTable(data = data_LEC,
                            number = as.numeric(strsplit(input$LEC_match_picker, " - ")[[1]][1]))

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
  }
})

