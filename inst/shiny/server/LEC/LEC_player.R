##### Third Box #####

# Player picker input
LEC_players <- reactive({
  temp <- unique(LEC_filt_team()$player)
  return(temp[temp != ""])
})

output$LEC_player <- renderUI({
  shinyWidgets::pickerInput("LEC_player", "Choose a player", choices = LEC_players(),
                            choicesOpt = list(content =  
                                                lapply(LEC_players(), FUN = function(player) {
                                                  HTML(paste(
                                                    tags$b(player)
                                                  ))
                                                })
                            ))
})

# Data filtered by player
LEC_filt_player <- reactive(LEC_filt_team() %>% filter(player %in% input$LEC_player))

stats_LEC_players_team_one <- reactive({
  stats_LEC_players_team() %>% filter(player == input$LEC_player)
})

##### First value boxes #####
# Nb of games
output$LEC_player_games <- renderValueBox({
  valueBox(
    nrow(LEC_filt_player()),
    "Games",
    icon = icon("gamepad")
  )
})

# Nb of wins
output$LEC_player_wins <- renderValueBox({
  valueBox(
    paste0(sum(LEC_filt_player()$result), "-",
           nrow(LEC_filt_player()) - sum(LEC_filt_player()$result)),
    "Wins-Losses",
    icon = icon("trophy"),
    color = "lime"
  )
})

# Winrate
output$LEC_player_winrate <- renderValueBox({
  valueBox(
    paste0(round(100*sum(LEC_filt_player()$result)/nrow(LEC_filt_player()),1),"%"),
    "Winrate",
    icon = icon("percent"),
    color = "fuchsia"
  )
})

##### UI - Box for advanced stats #####
output$LEC_advanced <- renderUI({
  box(
    title = paste0("Advanced Stats for ", input$LEC_team, " - ", input$LEC_player, 
                   ' (', unique(stats_LEC_players_team_one()$events), ")"),
    status = "primary", 
    width = 12,
    solidHeader = TRUE,
    column(width = 3,
           valueBoxOutput("LEC_player_champions", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LEC_player_games_det", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LEC_player_k_d_a", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LEC_player_kda", width = 12)
    ),
    column(width = 12,
           HTML("<h3><center> Filter by Champions: </center></h3>")),
    column(width = 3,
           uiOutput("LEC_champions")
    ),
    column(width = 3,
           valueBoxOutput("LEC_champion_games_det", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LEC_champion_k_d_a", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LEC_champion_kda", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LEC_champion_kp", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LEC_champion_ks", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LEC_champion_dth", width = 12)
    )
  )
})

##### SERVER - Box for advanced stats #####
# Datatable for champs played by player ordered by number of games
LEC_champs_played <- reactive({
  df <- as.data.frame(table(LEC_filt_player()$champion)) %>% 
    rename(Champion = Var1,
           Number = Freq) %>% 
    mutate(Champion = as.character(Champion))
  return(df[order(-df$Number),])
})

# Nb of champions played
output$LEC_player_champions <- renderValueBox({
  valueBox(
    nrow(LEC_champs_played()),
    "Champions played",
    icon = icon("user"),
    color = "navy"
  )
})

# Nb of wins, winrate for player
output$LEC_player_games_det <- renderValueBox({
  valueBox(
    paste0(nrow(LEC_filt_player())," (", sum(LEC_filt_player()$result), "-", 
           nrow(LEC_filt_player()) - sum(LEC_filt_player()$result), ") ",
           round(100*sum(LEC_filt_player()$result)/nrow(LEC_filt_player()), 1), "%"),
    "Games",
    icon = icon("gamepad"),
    color = "navy"
  )
})

# Player K-D-A
output$LEC_player_k_d_a <- renderValueBox({
  valueBox(
    paste0(sum(LEC_filt_player()$kills), "-", sum(LEC_filt_player()$deaths), "-",
           sum(LEC_filt_player()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

# Player KDA
output$LEC_player_kda <- renderValueBox({
  valueBox(
    calculate_kda(sum(LEC_filt_player()$kills),
                  sum(LEC_filt_player()$deaths),
                  sum(LEC_filt_player()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

# Picker input for champions played
output$LEC_champions <- renderUI({
  shinyWidgets::pickerInput("LEC_champions", "Champion",
                            choices = LEC_champs_played()$Champion,
                            selected = LEC_champs_played()$Champion,
                            multiple = TRUE,
                            options = shinyWidgets::pickerOptions(
                              actionsBox = TRUE,
                              liveSearch = TRUE,
                              countSelectedText = TRUE
                            ),
                            choicesOpt = list(content =  
                                                lapply(LEC_champs_played()$Champion, FUN = function(champ) {
                                                  HTML(paste(
                                                    tags$img(src=paste0("perso/", champ, ".png"),
                                                             width = 30,
                                                             height = 30),
                                                    tags$b(champ)
                                                  ))
                                                })
                            ))
})

# Data filtered by player and champion
LEC_filt_champion <- reactive(LEC_filt_player() %>% filter(champion %in% input$LEC_champions))

# Nb of wins, winrate for player on champion(s) selected 
output$LEC_champion_games_det <- renderValueBox({
  valueBox(
    paste0(nrow(LEC_filt_champion())," (", sum(LEC_filt_champion()$result), "-", 
           nrow(LEC_filt_champion()) - sum(LEC_filt_champion()$result), ") ",
           round(100*sum(LEC_filt_champion()$result)/nrow(LEC_filt_champion()), 1), "%"),
    "Games",
    icon = icon("gamepad"),
    color = "navy"
  )
})

# Player K-D-A on champion(s) selected
output$LEC_champion_k_d_a <- renderValueBox({
  valueBox(
    paste0(sum(LEC_filt_champion()$kills), "-", sum(LEC_filt_champion()$deaths), "-",
           sum(LEC_filt_champion()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

# Player KDA on champion(s) selected
output$LEC_champion_kda <- renderValueBox({
  valueBox(
    calculate_kda(sum(LEC_filt_champion()$kills),
                  sum(LEC_filt_champion()$deaths),
                  sum(LEC_filt_champion()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

# Player Kill Participation on champion(s) selected
output$LEC_champion_kp <- renderValueBox({
  valueBox(
    paste0(calculate_kp(sum(LEC_filt_champion()$kills),
                        sum(LEC_filt_champion()$assists),
                        sum(LEC_filt_champion()$teamkills)),"%"),
    "Kill Participation",
    icon = icon("percent"),
    color = "navy"
  )
})

# Player Kill Share on champion(s) selected
output$LEC_champion_ks <- renderValueBox({
  valueBox(
    paste0(calculate_ks(sum(LEC_filt_champion()$kills),
                        sum(LEC_filt_champion()$teamkills)),"%"),
    "Kill Share",
    icon = icon("percent"),
    color = "navy"
  )
})

# Player average share of team's deaths on champion(s) selected
output$LEC_champion_dth <- renderValueBox({
  valueBox(
    paste0(calculate_ds(sum(LEC_filt_champion()$deaths),
                        sum(LEC_filt_champion()$teamdeaths)), "%"),
    "Average share of team's deaths",
    icon = icon("percent"),
    color = "navy"
  )
})
