# Data filtered by event
LCK_filt_event <- reactive(data_LCK %>% filter(event %in% input$LCK_event))

##### Flags LCK
flags_LCK <- c(
  "AF.png",
  "DRX.png",
  "DWG.png",
  "BRO.png",
  "GEN.png",
  "HLE.png",
  "KT.png",
  "LSB.png",
  "NS.png",
  "T1.png"
)

flags_LCK <- data.frame(team = sort(unique(data_LCK$team)),
                        img = flags_LCK)

##### First Box #####
output$LCK_games <- renderValueBox({
  valueBox(
    nrow(LCK_filt_event())/12,
    "Games",
    icon = icon("gamepad")
  )
})

output$LCK_teams <- renderValueBox({
  valueBox(
    length(unique(LCK_filt_event()$team)),
    "Teams",
    icon = icon("users"),
    color = "lime"
  )
})

##### Second Box #####
output$LCK_team <- renderUI({
  shinyWidgets::pickerInput("LCK_team", "Team",
                            choices = sort(unique(LCK_filt_event()$team)),
                            choicesOpt = list(content =  
                                                lapply(sort(unique(LCK_filt_event()$team)), FUN = function(team) {
                                                  HTML(paste(
                                                    tags$img(src=flags_LCK[flags_LCK$team == team,]$img),
                                                    tags$b(team)
                                                  ))
                                                })
                            ))
})

# Data filtered by team
LCK_filt_team <- reactive(LCK_filt_event() %>% filter(team %in% input$LCK_team))

output$LCK_team_games <- renderValueBox({
  valueBox(
    nrow(LCK_filt_team())/6,
    "Games",
    icon = icon("gamepad")
  )
})

output$LCK_team_wins <- renderValueBox({
  valueBox(
    paste0(sum(LCK_filt_team()$result)/6, "-",
           nrow(LCK_filt_team())/6 - sum(LCK_filt_team()$result)/6),
    "Wins-Losses",
    icon = icon("trophy"),
    color = "lime"
  )
})

output$LCK_team_winrate <- renderValueBox({
  valueBox(
    paste0(round(100*sum(LCK_filt_team()$result)/nrow(LCK_filt_team()),1),"%"),
    "Winrate",
    icon = icon("percent"),
    color = "fuchsia"
  )
})

##### Third Box #####
LCK_players <- reactive({
  temp <- unique(LCK_filt_team()$player)
  return(temp[temp != ""])
})

output$LCK_player <- renderUI({
  shinyWidgets::pickerInput("LCK_player", "Player", choices = LCK_players(),
                            choicesOpt = list(content =  
                                                lapply(LCK_players(), FUN = function(player) {
                                                  HTML(paste(
                                                    tags$b(player)
                                                  ))
                                                })
                            ))
})

# Data filtered by player
LCK_filt_player <- reactive(LCK_filt_team() %>% filter(player %in% input$LCK_player))

output$LCK_player_games <- renderValueBox({
  valueBox(
    nrow(LCK_filt_player()),
    "Games",
    icon = icon("gamepad")
  )
})

output$LCK_player_wins <- renderValueBox({
  valueBox(
    paste0(sum(LCK_filt_player()$result), "-",
           nrow(LCK_filt_player()) - sum(LCK_filt_player()$result)),
    "Wins-Losses",
    icon = icon("trophy"),
    color = "lime"
  )
})

output$LCK_player_winrate <- renderValueBox({
  valueBox(
    paste0(round(100*sum(LCK_filt_player()$result)/nrow(LCK_filt_player()),1),"%"),
    "Winrate",
    icon = icon("percent"),
    color = "fuchsia"
  )
})

##### Forth Box #####
output$LCK_advanced <- renderUI({
  box(
    title = paste0("Advanced Stats for ", input$LCK_team, " - ", input$LCK_player),
    status = "primary", 
    width = 12,
    solidHeader = TRUE,
    column(width = 3,
           valueBoxOutput("LCK_player_champions", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LCK_player_games_det", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LCK_player_k_d_a", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LCK_player_kda", width = 12)
    ),
    column(width = 3,
           uiOutput("LCK_champions")
    ),
    column(width = 3,
           valueBoxOutput("LCK_champion_games_det", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LCK_champion_k_d_a", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LCK_champion_kda", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LCK_champion_kp", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LCK_champion_ks", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LCK_champion_dth", width = 12)
    )
    # ,
    # column(width = 12,
    #        plotOutput("radar", width = "50%")
    # )
  )
})

LCK_champs_played <- reactive({
  df <- as.data.frame(table(LCK_filt_player()$champion)) %>% 
    rename(Champion = Var1,
           Number = Freq) %>% 
    mutate(Champion = as.character(Champion))
  return(df[order(-df$Number),])
})

output$LCK_player_champions <- renderValueBox({
  valueBox(
    nrow(LCK_champs_played()),
    "Champions played",
    icon = icon("user"),
    color = "navy"
  )
})

output$LCK_player_games_det <- renderValueBox({
  valueBox(
    paste0(nrow(LCK_filt_player())," (", sum(LCK_filt_player()$result), "-", 
           nrow(LCK_filt_player()) - sum(LCK_filt_player()$result), ") ",
           round(100*sum(LCK_filt_player()$result)/nrow(LCK_filt_player()), 1), "%"),
    "Games",
    icon = icon("gamepad"),
    color = "navy"
  )
})

output$LCK_player_k_d_a <- renderValueBox({
  valueBox(
    paste0(sum(LCK_filt_player()$kills), "-", sum(LCK_filt_player()$deaths), "-",
           sum(LCK_filt_player()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

output$LCK_player_kda <- renderValueBox({
  valueBox(
    calculate_kda(sum(LCK_filt_player()$kills),
                  sum(LCK_filt_player()$deaths),
                  sum(LCK_filt_player()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

output$LCK_champions <- renderUI({
  shinyWidgets::pickerInput("LCK_champions", "Champion",
                            choices = LCK_champs_played()$Champion,
                            selected = LCK_champs_played()$Champion,
                            multiple = TRUE,
                            options = shinyWidgets::pickerOptions(
                              actionsBox = TRUE,
                              liveSearch = TRUE,
                              countSelectedText = TRUE
                            ),
                            choicesOpt = list(content =  
                                                lapply(LCK_champs_played()$Champion, FUN = function(champ) {
                                                  HTML(paste(
                                                    tags$img(src=paste0("perso/", champ, ".png"),
                                                             width = 30,
                                                             height = 30),
                                                    tags$b(champ)
                                                  ))
                                                })
                            ))
})

# Data filtered by player
LCK_filt_champion <- reactive(LCK_filt_player() %>% filter(champion %in% input$LCK_champions))

output$LCK_champion_games_det <- renderValueBox({
  valueBox(
    paste0(nrow(LCK_filt_champion())," (", sum(LCK_filt_champion()$result), "-", 
           nrow(LCK_filt_champion()) - sum(LCK_filt_champion()$result), ") ",
           round(100*sum(LCK_filt_champion()$result)/nrow(LCK_filt_champion()), 1), "%"),
    "Games",
    icon = icon("gamepad"),
    color = "navy"
  )
})

output$LCK_champion_k_d_a <- renderValueBox({
  valueBox(
    paste0(sum(LCK_filt_champion()$kills), "-", sum(LCK_filt_champion()$deaths), "-",
           sum(LCK_filt_champion()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

output$LCK_champion_kda <- renderValueBox({
  valueBox(
    calculate_kda(sum(LCK_filt_champion()$kills),
                  sum(LCK_filt_champion()$deaths),
                  sum(LCK_filt_champion()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

output$LCK_champion_kp <- renderValueBox({
  valueBox(
    paste0(calculate_kp(sum(LCK_filt_champion()$kills),
                        sum(LCK_filt_champion()$assists),
                        sum(LCK_filt_champion()$teamkills)),"%"),
    "Kill Participation",
    icon = icon("percent"),
    color = "navy"
  )
})

output$LCK_champion_ks <- renderValueBox({
  valueBox(
    paste0(calculate_ks(sum(LCK_filt_champion()$kills),
                        sum(LCK_filt_champion()$teamkills)),"%"),
    "Kill Share",
    icon = icon("percent"),
    color = "navy"
  )
})

output$LCK_champion_dth <- renderValueBox({
  valueBox(
    paste0(calculate_ds(sum(LCK_filt_champion()$deaths),
                        sum(LCK_filt_champion()$teamdeaths)), "%"),
    "Average share of team's deaths",
    icon = icon("percent"),
    color = "navy"
  )
})

# comparison_table <- reactive({
#   KP <- c(calculate_kp(sum(LCK_filt_player()$kills),
#                                sum(LCK_filt_player()$assists),
#                                sum(LCK_filt_player()$teamkills)),
#           calculate_kp(sum(LCK_filt_champion()$kills),
#                        sum(LCK_filt_champion()$assists),
#                        sum(LCK_filt_champion()$teamkills)),
#           100, 0)
#   
#   KS <- c(calculate_ks(sum(LCK_filt_player()$kills),
#                        sum(LCK_filt_player()$teamkills)),
#           calculate_ks(sum(LCK_filt_champion()$kills),
#                        sum(LCK_filt_champion()$teamkills)),
#           100, 0)
#     
#   AS <- c(calculate_ds(sum(LCK_filt_player()$deaths),
#                        sum(LCK_filt_player()$teamdeaths)),
#           calculate_ds(sum(LCK_filt_champion()$deaths),
#                        sum(LCK_filt_champion()$teamdeaths)),
#           100, 0)
#   
#   df <- data.frame(row.names = c("All champions",
#                                  "Selected champion(s)",
#                                  "Max",
#                                  "Min"),
#                    KP = KP, KS = KS, AS = AS)
#   return(df)
# })
# 
# output$radar <- renderPlot({
#   radarchart(comparison_table()[c("Max", "Min",
#                                   "All champions", "Selected champion(s)"),])
# })