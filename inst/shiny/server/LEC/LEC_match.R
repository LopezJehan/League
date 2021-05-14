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
  mutate(row = 1:nrow(blue_teams_LEC),
         Result = paste(row, "-", Blue, Blue_result,
                        "-", Red_result, Red))

flags_LEC_simplified <- flags_LEC %>% select(team, img)

matchs_LEC <- merge(matchs_LEC, flags_LEC_simplified,
                    by.x = "Blue", by.y = "team") %>% 
  rename("Blue_img" = img)

matchs_LEC <- merge(matchs_LEC, flags_LEC_simplified,
                    by.x = "Red", by.y = "team") %>% 
  rename("Red_img" = img) %>% 
  arrange(row)

# Picker Input
output$LEC_Match_picker <- renderUI(
  pickerInput("LEC_match_picker",
              "Choose a match",
              choices = matchs_LEC$Result,
              choicesOpt = list(content =  
                                  lapply(matchs_LEC$Result, FUN = function(result) {
                                    line <- matchs_LEC[matchs_LEC$Result == result,]
                                    result_break <- unlist(strsplit(result, " - "))
                                    HTML(paste0(
                                      tags$b(result_break[1]),
                                      tags$b(" - "),
                                      tags$img(src = line$Blue_img),
                                      tags$b(result_break[2]),
                                      tags$b(" - "),
                                      tags$b(result_break[3]),
                                      tags$img(src = line$Red_img)
                                    ))
                                  })
              )
  )
)

# Get match
LEC_match <- reactive(buildTable(data = data_LEC,
                                 number = as.numeric(strsplit(input$LEC_match_picker, " - ")[[1]][1])))

# Column HTML
LEC_sketch <- reactive({
  teamA <- colnames(LEC_match()[[1]][1])
  teamB <- colnames(LEC_match()[[1]][6])
  
  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(
          HTML(paste(
            tags$img(src=flags_LEC[flags_LEC$team == teamA,]$img),
            teamA
          ))
        ),
        th(""),
        th(""),
        th(colnames(LEC_match()[[1]][4])),
        th(colnames(LEC_match()[[1]][5])),
        th(""),
        th(""),
        th(
          HTML(paste(
            teamB,
            tags$img(src=flags_LEC[flags_LEC$team == teamB,]$img)
          ))
        )
      )
    )
  ))
})

# Display table
output$LEC_matches <- DT::renderDataTable({
  if(!is.null(input$LEC_match_picker)){
    DT::datatable(
      LEC_match()[[1]],
      options = list(dom = 't',
                     scrollX = TRUE,
                     ordering = FALSE,
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))),
      rownames = FALSE,
      caption =  htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: center;',
        htmltools::strong(LEC_match()[[2]])
      ),
      escape = FALSE,
      container = LEC_sketch()
    )
  }
})

