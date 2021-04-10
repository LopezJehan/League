tabItem(tabName = "LEC_competition",
        fluidRow(
          box(
            title = "Competition",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 6,
                   shinyWidgets::pickerInput("LEC_event", "Choose competitions",
                                             choices = sort(unique(data_LEC$event)),
                                             selected = sort(unique(data_LEC$event)),
                                             multiple = TRUE,
                                             options = list(
                                               "actions-box" = TRUE
                                             ),
                                             choicesOpt = list(content =  
                                                                 lapply(sort(unique(data_LPL$event)), FUN = function(event) {
                                                                   HTML(paste(
                                                                     tags$img(src="LEC.png", width = 40, height = 18),
                                                                     tags$b(event)
                                                                   ))
                                                                 })
                                             ))
            ),
            column(width = 3,
                   valueBoxOutput("LEC_games", width = 12)
            ),
            column(width = 3,
                   valueBoxOutput("LEC_teams", width = 12)
            )
          ),
          box(
            title = "Team comparison",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 12,
                   amChartsOutput("LEC_competition_team_graph")
            ),
            sidebar = boxSidebar(
              id = "LEC_competition_team_graph_sidebar",
              width = 25,
              pickerInput("LEC_competition_team_graph_choice",
                          "Choose a statistic",
                          choices = c("KDA", "Kills", "Assists", "Deaths",
                                      "Kills per games", "Assists per games",
                                      "Deaths per games"),
                          selected = "KDA")
            )
          ),
          box(
            title = "Player comparison",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 12,
                   amChartsOutput("LEC_competition_player_graph")
            )
          )
        )
)