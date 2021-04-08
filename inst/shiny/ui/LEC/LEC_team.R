tabItem(tabName = "LEC_team",
        fluidRow(
          box(
            title = "Team",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 3,
                   uiOutput("LEC_team")
            ),
            column(width = 3,
                   valueBoxOutput("LEC_team_games", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LEC_team_wins", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LEC_team_winrate", width = 12)
            )
            
          ),
          box(
            title = "Player",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 3,
                   uiOutput("LEC_player")
            ),
            column(width = 3,
                   valueBoxOutput("LEC_player_games", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LEC_player_wins", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LEC_player_winrate", width = 12)
            )
          ),
          uiOutput("LEC_advanced")
        )
)