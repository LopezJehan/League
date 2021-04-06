# Data filtered for team
data_team <- reactive(data_LPL %>% filter(team == input$team))
# Data filtered for team and with only team lines
data_only_team <- reactive(data_team() %>% filter(position == "team"))

##### Flags LPL
flags_LPL <- c(
  "BLG.png",
  "EDG.png",
  "ES.png",
  "FPX.png",
  "IG.png",
  "JDG.png",
  "LGD.png",
  "LNG.png",
  "OMG.png",
  "RA.png",
  "RW.png",
  "RNG.png",
  "SN.png",
  "WE.png",
  "TT.png",
  "TES.png",
  "V5.png"
)

flags_LPL <- data.frame(team = sort(unique(data_LPL$team)),
                        img = flags_LPL)

output$LPL_team <- renderUI({
  shinyWidgets::pickerInput("team", "Team",
                            choices = sort(unique(data_LPL$team)),
                            choicesOpt = list(content =  
                                                lapply(sort(unique(data_LPL$team)), FUN = function(team) {
                                                  HTML(paste(
                                                    tags$img(src=flags_LPL[flags_LPL$team == team,]$img),
                                                    team
                                                  ))
                                                })
                            ))
})

##### First Box - First Line #####
output$games_team <- renderValueBox({
  valueBox(
    nrow(data_only_team()),
    "Games",
    icon = icon("gamepad")
  )
})

output$wins_team <- renderValueBox({
  valueBox(
    sum(data_only_team()$result),
    "Wins",
    icon = icon("trophy"),
    color = "lime"
  )
})

output$winrate_team <- renderValueBox({
  valueBox(
    paste0(round(100*sum(data_only_team()$result)/nrow(data_only_team()),1),"%"),
    "Winrate",
    icon = icon("percent"),
    color = "fuchsia"
  )
})

##### First Box - Second Line #####
players <- reactive({
  temp <- unique(data_team()$player)
  return(temp[temp != ""])
})

output$player <- renderUI({
  shinyWidgets::pickerInput("player", "Player", choices = players())
})

# Data filtered for player
data_player <- reactive(data_team() %>% filter(player == input$player))

output$games_player <- renderValueBox({
  valueBox(
    nrow(data_player()),
    "Games",
    icon = icon("gamepad")
  )
})

output$wins_player <- renderValueBox({
  valueBox(
    sum(data_player()$result),
    "Wins",
    icon = icon("trophy"),
    color = "lime"
  )
})

output$winrate_player <- renderValueBox({
  valueBox(
    paste0(round(100*sum(data_player()$result)/nrow(data_player()),1), "%"),
    "Winrate",
    icon = icon("percent"),
    color = "fuchsia"
  )
})

##### Second Box - First Line #####
output$champions_player <- renderValueBox({
  valueBox(
    length(unique(data_player()$champion)),
    "Champions played",
    icon = icon("user"),
    color = "navy"
  )
})

output$games_player_2 <- renderValueBox({
  valueBox(
    paste0(nrow(data_player())," (", sum(data_player()$result), "-", 
          nrow(data_player()) - sum(data_player()$result), ") ",
          round(100*sum(data_player()$result)/nrow(data_player()), 1), "%"),
    "Games",
    icon = icon("gamepad"),
    color = "navy"
  )
})

output$k_d_a_player <- renderValueBox({
  valueBox(
    paste0(sum(data_player()$kills), "-", sum(data_player()$deaths), "-",
           sum(data_player()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

output$kda_player <- renderValueBox({
  valueBox(
    round((sum(data_player()$kills)+sum(data_player()$assists))/sum(data_player()$deaths),1),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

##### Second Box - Second Line #####
output$champion <- renderUI({
  shinyWidgets::pickerInput("champion", "Champion", multiple = TRUE,
                            choices = sort(unique(data_player()$champion)),
                            selected = sort(unique(data_player()$champion)),
                            options = shinyWidgets::pickerOptions(
                              actionsBox = TRUE,
                              liveSearch = TRUE,
                              countSelectedText = TRUE
                            ))
})

# Data filtered for player and champion
data_champion <- reactive(data_player() %>% filter(champion %in% input$champion))

output$games_champion <- renderValueBox({
  valueBox(
    paste0(nrow(data_champion())," (", sum(data_champion()$result), "-", 
           nrow(data_champion()) - sum(data_champion()$result), ") ",
           round(100*sum(data_champion()$result)/nrow(data_champion()), 1), "%"),
    "Games",
    icon = icon("gamepad"),
    color = "navy"
  )
})

output$k_d_a_champion <- renderValueBox({
  valueBox(
    paste0(sum(data_champion()$kills), "-", sum(data_champion()$deaths), "-",
           sum(data_champion()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

output$kda_champion <- renderValueBox({
  valueBox(
    calculate_kda(sum(data_champion()$kills),
                  sum(data_champion()$deaths),
                  sum(data_champion()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

##### Third Box - Second Line #####
output$kp_champion <- renderValueBox({
  valueBox(
    paste0(calculate_kp(sum(data_champion()$kills),
                        sum(data_champion()$assists),
                        sum(data_champion()$teamkills)),"%"),
    "Kill Participation",
    icon = icon("percent"),
    color = "navy"
  )
})

output$ks_champion <- renderValueBox({
  valueBox(
    paste0(calculate_ks(sum(data_champion()$kills),
                        sum(data_champion()$teamkills)),"%"),
    "Kill Share",
    icon = icon("percent"),
    color = "navy"
  )
})

output$dth_champion <- renderValueBox({
  valueBox(
    paste0(calculate_ds(sum(data_champion()$deaths),
                        sum(data_champion()$teamdeaths)),"%"),
    "Average share of team's deaths",
    icon = icon("percent"),
    color = "navy"
  )
})