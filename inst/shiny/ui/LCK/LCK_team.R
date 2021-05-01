tabItem(tabName = "LCK_team",
        fluidRow(
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
            title = "Player comparison",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 12,
                   amChartsOutput("LCK_team_player_graph")
            ),
            sidebar = boxSidebar(
              id = "LCK_team_player_graph_sidebar",
              width = 25,
              h4("Graphic Options"),
              pickerInput("LCK_team_player_graph_choice",
                          "Choose a statistic",
                          choices = c("KDA", "Kills", "Assists", "Deaths",
                                      "Kills per games", "Assists per games",
                                      "Deaths per games"),
                          selected = "KDA"),
              uiOutput("LCK_team_player_graph_slider")
            )
          )
        )
)