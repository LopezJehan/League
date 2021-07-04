tabItem(tabName = "LFL_player",
        fluidRow(
          box(
            title = "Player",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 3,
                   uiOutput("LFL_player")
            ),
            column(width = 3,
                   valueBoxOutput("LFL_player_games", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LFL_player_wins", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LFL_player_winrate", width = 12)
            )
          ),
          uiOutput("LFL_advanced")
        )
)