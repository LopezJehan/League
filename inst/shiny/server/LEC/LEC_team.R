##### Second Box #####
output$LEC_team <- renderUI({
  shinyWidgets::pickerInput("LEC_team", "Choose a team",
                            choices = sort(unique(LEC_filt_event()$team)),
                            choicesOpt = list(content =  
                                                lapply(sort(unique(LEC_filt_event()$team)), FUN = function(team) {
                                                  HTML(paste(
                                                    tags$img(src=flags_LEC[flags_LEC$team == team,]$img),
                                                    tags$b(team)
                                                  ))
                                                })
                            ))
})

# Data filtered by team
LEC_filt_team <- reactive(LEC_filt_event() %>% filter(team %in% input$LEC_team))

output$LEC_team_games <- renderValueBox({
  valueBox(
    nrow(LEC_filt_team())/6,
    "Games",
    icon = icon("gamepad")
  )
})

output$LEC_team_wins <- renderValueBox({
  valueBox(
    paste0(sum(LEC_filt_team()$result)/6, "-",
           nrow(LEC_filt_team())/6 - sum(LEC_filt_team()$result)/6),
    "Wins-Losses",
    icon = icon("trophy"),
    color = "lime"
  )
})

output$LEC_team_winrate <- renderValueBox({
  valueBox(
    paste0(round(100*sum(LEC_filt_team()$result)/nrow(LEC_filt_team()),1),"%"),
    "Winrate",
    icon = icon("percent"),
    color = "fuchsia"
  )
})