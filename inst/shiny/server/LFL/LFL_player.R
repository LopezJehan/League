##### Third Box #####

# Player picker input
LFL_players <- reactive({
  temp <- unique(LFL_filt_team()$player)
  return(temp[temp != ""])
})

output$LFL_player <- renderUI({
  shinyWidgets::pickerInput("LFL_player", "Choose a player", choices = LFL_players(),
                            choicesOpt = list(content =  
                                                lapply(LFL_players(), FUN = function(player) {
                                                  HTML(paste(
                                                    tags$b(player)
                                                  ))
                                                })
                            ))
})

# Data filtered by player
LFL_filt_player <- reactive(LFL_filt_team() %>% filter(player %in% input$LFL_player))

stats_LFL_players_team_one <- reactive({
  stats_LFL_players_team() %>% filter(player == input$LFL_player)
})

##### First value boxes #####
# Nb of games
output$LFL_player_games <- renderValueBox({
  valueBox(
    nrow(LFL_filt_player()),
    "Games",
    icon = icon("gamepad")
  )
})

# Nb of wins
output$LFL_player_wins <- renderValueBox({
  valueBox(
    paste0(sum(LFL_filt_player()$result), "-",
           nrow(LFL_filt_player()) - sum(LFL_filt_player()$result)),
    "Wins-Losses",
    icon = icon("trophy"),
    color = "lime"
  )
})

# Winrate
output$LFL_player_winrate <- renderValueBox({
  valueBox(
    paste0(round(100*sum(LFL_filt_player()$result)/nrow(LFL_filt_player()),1),"%"),
    "Winrate",
    icon = icon("percent"),
    color = "fuchsia"
  )
})

##### UI - Box for advanced stats #####
output$LFL_advanced <- renderUI({
  box(
    title = paste0("Advanced Stats for ", input$LFL_team, " - ", input$LFL_player, 
                   ' (', unique(stats_LFL_players_team_one()$events), ")"),
    status = "primary", 
    width = 12,
    solidHeader = TRUE,
    column(width = 3,
           valueBoxOutput("LFL_player_champions", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LFL_player_games_det", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LFL_player_k_d_a", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LFL_player_kda", width = 12)
    ),
    column(width = 12,
           HTML("<h3><center> Filter by Champions: </center></h3>")),
    column(width = 3,
           uiOutput("LFL_champions")
    ),
    column(width = 3,
           valueBoxOutput("LFL_champion_games_det", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LFL_champion_k_d_a", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LFL_champion_kda", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LFL_champion_kp", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LFL_champion_ks", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LFL_champion_dth", width = 12)
    )
  )
})

##### SERVER - Box for advanced stats #####
# Datatable for champs played by player ordered by number of games
LFL_champs_played <- reactive({
  df <- as.data.frame(table(LFL_filt_player()$champion)) %>% 
    rename(Champion = Var1,
           Number = Freq) %>% 
    mutate(Champion = as.character(Champion))
  return(df[order(-df$Number),])
})

# Nb of champions played
output$LFL_player_champions <- renderValueBox({
  valueBox(
    nrow(LFL_champs_played()),
    "Champions played",
    icon = icon("user"),
    color = "navy"
  )
})

# Nb of wins, winrate for player
output$LFL_player_games_det <- renderValueBox({
  valueBox(
    paste0(nrow(LFL_filt_player())," (", sum(LFL_filt_player()$result), "-", 
           nrow(LFL_filt_player()) - sum(LFL_filt_player()$result), ") ",
           round(100*sum(LFL_filt_player()$result)/nrow(LFL_filt_player()), 1), "%"),
    "Games",
    icon = icon("gamepad"),
    color = "navy"
  )
})

# Player K-D-A
output$LFL_player_k_d_a <- renderValueBox({
  valueBox(
    paste0(sum(LFL_filt_player()$kills), "-", sum(LFL_filt_player()$deaths), "-",
           sum(LFL_filt_player()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

# Player KDA
output$LFL_player_kda <- renderValueBox({
  valueBox(
    calculate_kda(sum(LFL_filt_player()$kills),
                  sum(LFL_filt_player()$deaths),
                  sum(LFL_filt_player()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

# Picker input for champions played
output$LFL_champions <- renderUI({
  shinyWidgets::pickerInput("LFL_champions", "Champion",
                            choices = LFL_champs_played()$Champion,
                            selected = LFL_champs_played()$Champion,
                            multiple = TRUE,
                            options = shinyWidgets::pickerOptions(
                              actionsBox = TRUE,
                              liveSearch = TRUE,
                              countSelectedText = TRUE
                            ),
                            choicesOpt = list(content =  
                                                lapply(LFL_champs_played()$Champion, FUN = function(champ) {
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
LFL_filt_champion <- reactive(LFL_filt_player() %>% filter(champion %in% input$LFL_champions))

# Nb of wins, winrate for player on champion(s) selected 
output$LFL_champion_games_det <- renderValueBox({
  valueBox(
    paste0(nrow(LFL_filt_champion())," (", sum(LFL_filt_champion()$result), "-", 
           nrow(LFL_filt_champion()) - sum(LFL_filt_champion()$result), ") ",
           round(100*sum(LFL_filt_champion()$result)/nrow(LFL_filt_champion()), 1), "%"),
    "Games",
    icon = icon("gamepad"),
    color = "navy"
  )
})

# Player K-D-A on champion(s) selected
output$LFL_champion_k_d_a <- renderValueBox({
  valueBox(
    paste0(sum(LFL_filt_champion()$kills), "-", sum(LFL_filt_champion()$deaths), "-",
           sum(LFL_filt_champion()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

# Player KDA on champion(s) selected
output$LFL_champion_kda <- renderValueBox({
  valueBox(
    calculate_kda(sum(LFL_filt_champion()$kills),
                  sum(LFL_filt_champion()$deaths),
                  sum(LFL_filt_champion()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

# Player Kill Participation on champion(s) selected
output$LFL_champion_kp <- renderValueBox({
  valueBox(
    paste0(calculate_kp(sum(LFL_filt_champion()$kills),
                        sum(LFL_filt_champion()$assists),
                        sum(LFL_filt_champion()$teamkills)),"%"),
    "Kill Participation",
    icon = icon("percent"),
    color = "navy"
  )
})

# Player Kill Share on champion(s) selected
output$LFL_champion_ks <- renderValueBox({
  valueBox(
    paste0(calculate_ks(sum(LFL_filt_champion()$kills),
                        sum(LFL_filt_champion()$teamkills)),"%"),
    "Kill Share",
    icon = icon("percent"),
    color = "navy"
  )
})

# Player average share of team's deaths on champion(s) selected
output$LFL_champion_dth <- renderValueBox({
  valueBox(
    paste0(calculate_ds(sum(LFL_filt_champion()$deaths),
                        sum(LFL_filt_champion()$teamdeaths)), "%"),
    "Average share of team's deaths",
    icon = icon("percent"),
    color = "navy"
  )
})
