tabItem(tabName = "LPL",
        fluidRow(
          box(
            title = "Team and Player",
            status = "primary", 
            width = 12,
            column(width = 3,
                   uiOutput("LPL_team")
            ),
            column(width = 3,
                   valueBoxOutput("games_team", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("wins_team", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("winrate_team", width = 12)
            ),
            column(width = 3,
                   uiOutput("player")
            ),
            column(width = 3,
                   valueBoxOutput("games_player", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("wins_player", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("winrate_player", width = 12)
            )
          )
        ),
        fluidRow(
          box(
            title = "Advanced Stats for Player",
            status = "primary", 
            width = 12,
            column(width = 3,
                   valueBoxOutput("champions_player", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("games_player_2", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("k_d_a_player", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("kda_player", width = 12)
            ),
            column(width = 3,
                   uiOutput("champion")
            ),
            column(width = 3,
                   valueBoxOutput("games_champion", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("k_d_a_champion", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("kda_champion", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("kp_champion", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("ks_champion", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("dth_champion", width = 12)
            )
          )
        )
)