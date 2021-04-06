# Data filtered by event
LPL_filt_event <- reactive(data_LPL %>% filter(event %in% input$LPL_event))

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

##### First Box #####
output$LPL_games <- renderValueBox({
  valueBox(
    nrow(LPL_filt_event())/12,
    "Games",
    icon = icon("gamepad")
  )
})

output$LPL_teams <- renderValueBox({
  valueBox(
    length(unique(LPL_filt_event()$team)),
    "Teams",
    icon = icon("users"),
    color = "lime"
  )
})

##### Second Box #####
output$LPL_team <- renderUI({
  shinyWidgets::pickerInput("LPL_team", "Team",
                            choices = sort(unique(LPL_filt_event()$team)),
                            choicesOpt = list(content =  
                                                lapply(sort(unique(LPL_filt_event()$team)), FUN = function(team) {
                                                  HTML(paste(
                                                    tags$img(src=flags_LPL[flags_LPL$team == team,]$img),
                                                    tags$b(team)
                                                  ))
                                                })
                            ))
})

# Data filtered by team
LPL_filt_team <- reactive(LPL_filt_event() %>% filter(team %in% input$LPL_team))

output$LPL_team_games <- renderValueBox({
  valueBox(
    nrow(LPL_filt_team())/6,
    "Games",
    icon = icon("gamepad")
  )
})

output$LPL_team_wins <- renderValueBox({
  valueBox(
    paste0(sum(LPL_filt_team()$result)/6, "-",
           nrow(LPL_filt_team())/6 - sum(LPL_filt_team()$result)/6),
    "Wins-Losses",
    icon = icon("trophy"),
    color = "lime"
  )
})

output$LPL_team_winrate <- renderValueBox({
  valueBox(
    paste0(round(100*sum(LPL_filt_team()$result)/nrow(LPL_filt_team()),1),"%"),
    "Winrate",
    icon = icon("percent"),
    color = "fuchsia"
  )
})

##### Third Box #####
LPL_players <- reactive({
  temp <- unique(LPL_filt_team()$player)
  return(temp[temp != ""])
})

output$LPL_player <- renderUI({
  shinyWidgets::pickerInput("LPL_player", "Player", choices = LPL_players(),
                            choicesOpt = list(content =  
                                                lapply(LPL_players(), FUN = function(player) {
                                                  HTML(paste(
                                                    tags$b(player)
                                                  ))
                                                })
                            ))
})

# Data filtered by player
LPL_filt_player <- reactive(LPL_filt_team() %>% filter(player %in% input$LPL_player))

output$LPL_player_games <- renderValueBox({
  valueBox(
    nrow(LPL_filt_player()),
    "Games",
    icon = icon("gamepad")
  )
})

output$LPL_player_wins <- renderValueBox({
  valueBox(
    paste0(sum(LPL_filt_player()$result), "-",
           nrow(LPL_filt_player()) - sum(LPL_filt_player()$result)),
    "Wins-Losses",
    icon = icon("trophy"),
    color = "lime"
  )
})

output$LPL_player_winrate <- renderValueBox({
  valueBox(
    paste0(round(100*sum(LPL_filt_player()$result)/nrow(LPL_filt_player()),1),"%"),
    "Winrate",
    icon = icon("percent"),
    color = "fuchsia"
  )
})

##### Forth Box #####
output$LPL_advanced <- renderUI({
  box(
    title = paste0("Advanced Stats for ", input$LPL_team, " - ", input$LPL_player),
    status = "primary", 
    width = 12,
    solidHeader = TRUE,
    column(width = 3,
           valueBoxOutput("LPL_player_champions", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LPL_player_games_det", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LPL_player_k_d_a", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LPL_player_kda", width = 12)
    ),
    column(width = 3,
           uiOutput("LPL_champions")
    ),
    column(width = 3,
           valueBoxOutput("LPL_champion_games_det", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LPL_champion_k_d_a", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LPL_champion_kda", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LPL_champion_kp", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LPL_champion_ks", width = 12)
    ),
    column(width = 3,
           valueBoxOutput("LPL_champion_dth", width = 12)
    )
    # ,
    # column(width = 12,
    #        plotOutput("radar", width = "50%")
    # )
  )
})

LPL_champs_played <- reactive({
  df <- as.data.frame(table(LPL_filt_player()$champion)) %>% 
    rename(Champion = Var1,
           Number = Freq) %>% 
    mutate(Champion = as.character(Champion))
  return(df[order(-df$Number),])
})

output$LPL_player_champions <- renderValueBox({
  valueBox(
    nrow(LPL_champs_played()),
    "Champions played",
    icon = icon("user"),
    color = "navy"
  )
})

output$LPL_player_games_det <- renderValueBox({
  valueBox(
    paste0(nrow(LPL_filt_player())," (", sum(LPL_filt_player()$result), "-", 
           nrow(LPL_filt_player()) - sum(LPL_filt_player()$result), ") ",
           round(100*sum(LPL_filt_player()$result)/nrow(LPL_filt_player()), 1), "%"),
    "Games",
    icon = icon("gamepad"),
    color = "navy"
  )
})

output$LPL_player_k_d_a <- renderValueBox({
  valueBox(
    paste0(sum(LPL_filt_player()$kills), "-", sum(LPL_filt_player()$deaths), "-",
           sum(LPL_filt_player()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

output$LPL_player_kda <- renderValueBox({
  valueBox(
    calculate_kda(sum(LPL_filt_player()$kills),
                  sum(LPL_filt_player()$deaths),
                  sum(LPL_filt_player()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

output$LPL_champions <- renderUI({
  shinyWidgets::pickerInput("LPL_champions", "Champion",
                            choices = LPL_champs_played()$Champion,
                            selected = LPL_champs_played()$Champion,
                            multiple = TRUE,
                            options = shinyWidgets::pickerOptions(
                              actionsBox = TRUE,
                              liveSearch = TRUE,
                              countSelectedText = TRUE
                            ),
                            choicesOpt = list(content =  
                                                lapply(LPL_champs_played()$Champion, FUN = function(champ) {
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
LPL_filt_champion <- reactive(LPL_filt_player() %>% filter(champion %in% input$LPL_champions))

output$LPL_champion_games_det <- renderValueBox({
  valueBox(
    paste0(nrow(LPL_filt_champion())," (", sum(LPL_filt_champion()$result), "-", 
           nrow(LPL_filt_champion()) - sum(LPL_filt_champion()$result), ") ",
           round(100*sum(LPL_filt_champion()$result)/nrow(LPL_filt_champion()), 1), "%"),
    "Games",
    icon = icon("gamepad"),
    color = "navy"
  )
})

output$LPL_champion_k_d_a <- renderValueBox({
  valueBox(
    paste0(sum(LPL_filt_champion()$kills), "-", sum(LPL_filt_champion()$deaths), "-",
           sum(LPL_filt_champion()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

output$LPL_champion_kda <- renderValueBox({
  valueBox(
    calculate_kda(sum(LPL_filt_champion()$kills),
                  sum(LPL_filt_champion()$deaths),
                  sum(LPL_filt_champion()$assists)),
    "KDA",
    icon = icon("percent"),
    color = "navy"
  )
})

output$LPL_champion_kp <- renderValueBox({
  valueBox(
    paste0(calculate_kp(sum(LPL_filt_champion()$kills),
                        sum(LPL_filt_champion()$assists),
                        sum(LPL_filt_champion()$teamkills)),"%"),
    "Kill Participation",
    icon = icon("percent"),
    color = "navy"
  )
})

output$LPL_champion_ks <- renderValueBox({
  valueBox(
    paste0(calculate_ks(sum(LPL_filt_champion()$kills),
                        sum(LPL_filt_champion()$teamkills)),"%"),
    "Kill Share",
    icon = icon("percent"),
    color = "navy"
  )
})

output$LPL_champion_dth <- renderValueBox({
  valueBox(
    paste0(calculate_ds(sum(LPL_filt_champion()$deaths),
                        sum(LPL_filt_champion()$teamdeaths)), "%"),
    "Average share of team's deaths",
    icon = icon("percent"),
    color = "navy"
  )
})

# comparison_table <- reactive({
#   KP <- c(calculate_kp(sum(LPL_filt_player()$kills),
#                                sum(LPL_filt_player()$assists),
#                                sum(LPL_filt_player()$teamkills)),
#           calculate_kp(sum(LPL_filt_champion()$kills),
#                        sum(LPL_filt_champion()$assists),
#                        sum(LPL_filt_champion()$teamkills)),
#           100, 0)
#   
#   KS <- c(calculate_ks(sum(LPL_filt_player()$kills),
#                        sum(LPL_filt_player()$teamkills)),
#           calculate_ks(sum(LPL_filt_champion()$kills),
#                        sum(LPL_filt_champion()$teamkills)),
#           100, 0)
#     
#   AS <- c(calculate_ds(sum(LPL_filt_player()$deaths),
#                        sum(LPL_filt_player()$teamdeaths)),
#           calculate_ds(sum(LPL_filt_champion()$deaths),
#                        sum(LPL_filt_champion()$teamdeaths)),
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