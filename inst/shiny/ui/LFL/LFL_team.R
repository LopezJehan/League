tabItem(tabName = "LFL_team",
        fluidRow(
          box(
            title = "Team",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 3,
                   uiOutput("LFL_team")
            ),
            column(width = 3,
                   valueBoxOutput("LFL_team_games", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LFL_team_wins", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LFL_team_winrate", width = 12)
            )
          ),
          box(
            title = "Player comparison",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 12,
                   amChartsOutput("LFL_team_player_graph")
            ),
            sidebar = boxSidebar(
              id = "LFL_team_player_graph_sidebar",
              width = 25,
              h4("Graphic Options"),
              pickerInput("LFL_team_player_graph_choice",
                          "Choose a statistic",
                          choices = c("KDA", "Kills", "Assists", "Deaths",
                                      "Kills per games", "Assists per games",
                                      "Deaths per games"),
                          selected = "KDA"),
              uiOutput("LFL_team_player_graph_slider")
            )
          )
        )
)