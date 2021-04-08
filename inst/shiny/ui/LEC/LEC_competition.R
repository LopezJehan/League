tabItem(tabName = "LEC_competition",
        fluidRow(
          box(
            title = "Competition",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 6,
                   # shinyWidgets::checkboxGroupButtons(
                   #   "LEC_event", "Competitions",
                   #   choices = sort(unique(data_LEC$event))
                   # )
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
          )
        )
)