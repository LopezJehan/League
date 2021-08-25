##### LCK match server #####
blue_teams_LCK <- data_LCK %>% 
  filter(position == "team",
         side == "Blue") %>% 
  select(team, result, event)

red_teams_LCK <- data_LCK %>% 
  filter(position == "team",
         side == "Red") %>%
  select(team, result) 

matchs_LCK <- data.frame(blue_teams_LCK, red_teams_LCK) %>% 
  rename("Blue" = team,
         "Red" = team.1,
         "Blue_result" = result,
         "Red_result" = result.1) %>% 
  mutate(row = 1:nrow(blue_teams_LCK),
         Result = paste(row, "-", Blue, Blue_result,
                        "-", Red_result, Red))

flags_LCK_simplified <- flags_LCK %>% select(team, img)

matchs_LCK <- merge(matchs_LCK, flags_LCK_simplified,
                    by.x = "Blue", by.y = "team") %>% 
  rename("Blue_img" = img)

matchs_LCK <- merge(matchs_LCK, flags_LCK_simplified,
                    by.x = "Red", by.y = "team") %>% 
  rename("Red_img" = img) %>% 
  arrange(row)

output$LCK_Match_team_A <- renderUI(
  shinyWidgets::pickerInput("LCK_Match_team_A", "Choose a team",
                            choices = sort(unique(data_LCK$team)),
                            selected = sort(unique(data_LCK$team)),
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE,
                                           `selected-text-format` = paste0("count > ", length(unique(data_LCK$team))-1),
                                           `count-selected-text` = "All teams selected"),
                            choicesOpt = list(content =  
                                                lapply(sort(unique(data_LCK$team)), FUN = function(team) {
                                                  HTML(paste(
                                                    tags$img(src=flags_LCK[flags_LCK$team == team,]$img),
                                                    tags$b(team)
                                                  ))
                                                })
                            ))
)

output$LCK_Match_team_B <- renderUI(
  shinyWidgets::pickerInput("LCK_Match_team_B", "Choose another team",
                            choices = sort(unique(data_LCK$team)),
                            selected = sort(unique(data_LCK$team)),
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE,
                                           `selected-text-format` = paste0("count > ", length(unique(data_LCK$team))-1),
                                           `count-selected-text` = "All teams selected"),
                            choicesOpt = list(content =  
                                                lapply(sort(unique(data_LCK$team)), FUN = function(team) {
                                                  HTML(paste(
                                                    tags$img(src=flags_LCK[flags_LCK$team == team,]$img),
                                                    tags$b(team)
                                                  ))
                                                })
                            ))
)

# Filter matches
matchs_filtered <- reactive({
  matchs_LCK %>% 
    filter(event %in% input$LCK_match_event,
           Red %in% input$LCK_Match_team_A | Blue %in% input$LCK_Match_team_A) %>% 
    filter(Red %in% input$LCK_Match_team_B | Blue %in% input$LCK_Match_team_B)
})

# Picker Input
output$LCK_Match_picker <- renderUI(
  pickerInput("LCK_match_picker",
              "Choose a match",
              choices = matchs_filtered()$Result,
              choicesOpt = list(content =  
                                  lapply(matchs_filtered()$Result, FUN = function(result) {
                                    line <- matchs_filtered()[matchs_filtered()$Result == result,]
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
LCK_match <- reactive(buildTable(data = data_LCK,
                                 number = as.numeric(strsplit(input$LCK_match_picker, " - ")[[1]][1])))

# Column HTML
LCK_sketch <- reactive({
  teamA <- colnames(LCK_match()[[1]][1])
  teamB <- colnames(LCK_match()[[1]][6])
  
  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(
          HTML(paste(
            tags$img(src=flags_LCK[flags_LCK$team == teamA,]$img),
            teamA
          ))
        ),
        th(""),
        th(""),
        th(colnames(LCK_match()[[1]][4])),
        th(colnames(LCK_match()[[1]][5])),
        th(""),
        th(""),
        th(
          HTML(paste(
            teamB,
            tags$img(src=flags_LCK[flags_LCK$team == teamB,]$img)
          ))
        )
      )
    )
  ))
})

# Display table
output$LCK_matches <- DT::renderDataTable({
  if(!is.null(input$LCK_match_picker)){
    DT::datatable(
      LCK_match()[[1]],
      options = list(dom = 't',
                     scrollX = TRUE,
                     ordering = FALSE,
                     columnDefs = list(list(className = 'dt-center', targets = "_all"))),
      rownames = FALSE,
      caption =  htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: center;',
        htmltools::strong(LCK_match()[[2]])
      ),
      escape = FALSE,
      container = LCK_sketch()
    )
  }
})

