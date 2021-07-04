##### LFL match server #####
blue_teams_LFL <- data_LFL %>% 
  filter(position == "team",
         side == "Blue") %>% 
  select(team, result)

red_teams_LFL <- data_LFL %>% 
  filter(position == "team",
         side == "Red") %>%
  select(team, result) 

matchs_LFL <- data.frame(blue_teams_LFL, red_teams_LFL) %>% 
  rename("Blue" = team,
         "Red" = team.1,
         "Blue_result" = result,
         "Red_result" = result.1) %>% 
  mutate(row = 1:nrow(blue_teams_LFL),
         Result = paste(row, "-", Blue, Blue_result,
                        "-", Red_result, Red))

flags_LFL_simplified <- flags_LFL %>% select(team, img)

matchs_LFL <- merge(matchs_LFL, flags_LFL_simplified,
                    by.x = "Blue", by.y = "team") %>% 
  rename("Blue_img" = img)

matchs_LFL <- merge(matchs_LFL, flags_LFL_simplified,
                    by.x = "Red", by.y = "team") %>% 
  rename("Red_img" = img) %>% 
  arrange(row)

# Picker Input
output$LFL_Match_picker <- renderUI(
  pickerInput("LFL_match_picker",
              "Choose a match",
              choices = matchs_LFL$Result,
              choicesOpt = list(content =  
                                  lapply(matchs_LFL$Result, FUN = function(result) {
                                    line <- matchs_LFL[matchs_LFL$Result == result,]
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
LFL_match <- reactive(buildTable(data = data_LFL,
                                 number = as.numeric(strsplit(input$LFL_match_picker, " - ")[[1]][1])))

# Column HTML
LFL_sketch <- reactive({
  teamA <- colnames(LFL_match()[[1]][1])
  teamB <- colnames(LFL_match()[[1]][6])
  
  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(
          HTML(paste(
            tags$img(src=flags_LFL[flags_LFL$team == teamA,]$img),
            teamA
          ))
        ),
        th(""),
        th(""),
        th(colnames(LFL_match()[[1]][4])),
        th(colnames(LFL_match()[[1]][5])),
        th(""),
        th(""),
        th(
          HTML(paste(
            teamB,
            tags$img(src=flags_LFL[flags_LFL$team == teamB,]$img)
          ))
        )
      )
    )
  ))
})

# Display table
output$LFL_matches <- DT::renderDataTable({
  if(!is.null(input$LFL_match_picker)){
    DT::datatable(
      LFL_match()[[1]],
      options = list(dom = 't',
                     scrollX = TRUE,
                     ordering = FALSE,
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))),
      rownames = FALSE,
      caption =  htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: center;',
        htmltools::strong(LFL_match()[[2]])
      ),
      escape = FALSE,
      container = LFL_sketch()
    )
  }
})

