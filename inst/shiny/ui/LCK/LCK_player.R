tabItem(tabName = "LCK_player",
        fluidRow(
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