tabItem(tabName = "LFL_competition",
        fluidRow(
          box(
            title = "Competition",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 6,
                   shinyWidgets::pickerInput("LFL_event", "Choose competitions",
                                             choices = unique(data_LFL$event),
                                             selected = unique(data_LFL$event),
                                             multiple = TRUE,
                                             options = list(
                                               "actions-box" = TRUE
                                             ),
                                             choicesOpt = list(content =  
                                                                 lapply(unique(data_LFL$event), FUN = function(event) {
                                                                   HTML(paste(
                                                                     tags$img(src="LFL.png", width = 32, height = 28),
                                                                     tags$b(event)
                                                                   ))
                                                                 })
                                             ))
            ),
            column(width = 3,
                   valueBoxOutput("LFL_games", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LFL_teams", width = 12)
            )
          ),
          box(
            title = "Team comparison",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 12,
                   amChartsOutput("LFL_competition_team_graph")
            ),
            sidebar = boxSidebar(
              id = "LFL_competition_team_graph_sidebar",
              width = 25,
              h4("Graphic Options"),
              pickerInput("LFL_competition_team_graph_choice",
                          "Choose a statistic",
                          choices = c("KDA", "Kills", "Assists", "Deaths",
                                      "Kills per games", "Assists per games",
                                      "Deaths per games"),
                          selected = "KDA"),
              uiOutput("LFL_competition_team_graph_slider")
            )
          ),
          box(
            title = "Player comparison",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 12,
                   amChartsOutput("LFL_competition_player_graph")
            ),
            sidebar = boxSidebar(
              id = "LFL_competition_player_graph_sidebar",
              width = 25,
              h4("Graphic Options"),
              pickerInput("LFL_competition_player_graph_choice",
                          "Choose a statistic",
                          choices = c("KDA", "Kills", "Assists", "Deaths",
                                      "Kills per games", "Assists per games",
                                      "Deaths per games"),
                          selected = "KDA"),
              uiOutput("LFL_competition_player_graph_slider")
            )
          )
        )
)