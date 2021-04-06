tabItem(tabName = "LPL",
        fluidRow(
          box(
            title = "Competition",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 6,
                   # shinyWidgets::checkboxGroupButtons(
                   #   "LPL_event", "Competitions",
                   #   choices = sort(unique(data_LPL$event))
                   # )
                   shinyWidgets::pickerInput("LPL_event", "Competitions",
                                             choices = sort(unique(data_LPL$event)),
                                             selected = sort(unique(data_LPL$event)),
                                             multiple = TRUE,
                                             options = list(
                                               "actions-box" = TRUE
                                             ),
                                             choicesOpt = list(content =  
                                                                 lapply(sort(unique(data_LPL$event)), FUN = function(event) {
                                                                   HTML(paste(
                                                                     tags$img(src="LPL.png", width = 40, height = 24),
                                                                     tags$b(event)
                                                                   ))
                                                                 })
                                             ))
            ),
            column(width = 3,
                   valueBoxOutput("LPL_games", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LPL_teams", width = 12)
            )
          ),
          box(
            title = "Team",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 3,
                   uiOutput("LPL_team")
            ),
            column(width = 3,
                   valueBoxOutput("LPL_team_games", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LPL_team_wins", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LPL_team_winrate", width = 12)
            )
            
          ),
          box(
            title = "Player",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 3,
                   uiOutput("LPL_player")
            ),
            column(width = 3,
                   valueBoxOutput("LPL_player_games", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LPL_player_wins", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LPL_player_winrate", width = 12)
            )
          ),
          uiOutput("LPL_advanced")
        )
)