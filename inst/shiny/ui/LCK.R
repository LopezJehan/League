tabItem(tabName = "LCK",
        fluidRow(
          box(
            title = "Competition",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 6,
                   # shinyWidgets::checkboxGroupButtons(
                   #   "LCK_event", "Competitions",
                   #   choices = sort(unique(data_LCK$event))
                   # )
                   shinyWidgets::pickerInput("LCK_event", "Competitions",
                                             choices = sort(unique(data_LCK$event)),
                                             selected = sort(unique(data_LCK$event)),
                                             multiple = TRUE,
                                             options = list(
                                               "actions-box" = TRUE
                                             ),
                                             choicesOpt = list(content =  
                                                                 lapply(sort(unique(data_LPL$event)), FUN = function(event) {
                                                                   HTML(paste(
                                                                     tags$img(src="LCK.png", width = 40, height = 28),
                                                                     tags$b(event)
                                                                   ))
                                                                 })
                                             ))
            ),
            column(width = 3,
                   valueBoxOutput("LCK_games", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LCK_teams", width = 12)
            )
          ),
          box(
            title = "Team",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 3,
                   uiOutput("LCK_team")
            ),
            column(width = 3,
                   valueBoxOutput("LCK_team_games", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LCK_team_wins", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LCK_team_winrate", width = 12)
            )
            
          ),
          box(
            title = "Player",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 3,
                   uiOutput("LCK_player")
            ),
            column(width = 3,
                   valueBoxOutput("LCK_player_games", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LCK_player_wins", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LCK_player_winrate", width = 12)
            )
          ),
          uiOutput("LCK_advanced")
        )
)