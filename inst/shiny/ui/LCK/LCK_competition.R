tabItem(tabName = "LCK_competition",
        fluidRow(
          box(
            title = "Competition",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 6,
                   shinyWidgets::pickerInput("LCK_event", "Choose competitions",
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
            title = "Team comparison",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 12,
                   amChartsOutput("LCK_competition_team_graph")
            ),
            sidebar = boxSidebar(
              id = "LCK_competition_team_graph_sidebar",
              width = 25,
              h4("Graphic Options"),
              pickerInput("LCK_competition_team_graph_choice",
                          "Choose a statistic",
                          choices = c("KDA", "Kills", "Assists", "Deaths",
                                      "Kills per games", "Assists per games",
                                      "Deaths per games"),
                          selected = "KDA"),
              uiOutput("LCK_competition_team_graph_slider")
            )
          ),
          box(
            title = "Player comparison",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 12,
                   amChartsOutput("LCK_competition_player_graph")
            ),
            sidebar = boxSidebar(
              id = "LCK_competition_player_graph_sidebar",
              width = 25,
              h4("Graphic Options"),
              pickerInput("LCK_competition_player_graph_choice",
                          "Choose a statistic",
                          choices = c("KDA", "Kills", "Assists", "Deaths",
                                      "Kills per games", "Assists per games",
                                      "Deaths per games"),
                          selected = "KDA"),
              uiOutput("LCK_competition_player_graph_slider")
            )
          )
        )
)