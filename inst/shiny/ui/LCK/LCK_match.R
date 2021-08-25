tabItem(tabName = "LCK_match",
        fluidRow(
          box(
            title = "Match",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 6,
                   shinyWidgets::pickerInput("LCK_match_event", "Choose competitions",
                                             choices = sort(unique(data_LCK$event)),
                                             selected = sort(unique(data_LCK$event)),
                                             multiple = TRUE,
                                             options = list(
                                               "actions-box" = TRUE
                                             ),
                                             choicesOpt = list(content =  
                                                                 lapply(sort(unique(data_LCK$event)), FUN = function(event) {
                                                                   HTML(paste(
                                                                     tags$img(src="LCK.png", width = 40, height = 28),
                                                                     tags$b(event)
                                                                   ))
                                                                 })
                                             ))
            ),
            column(width = 3,
                   uiOutput("LCK_Match_team_A")
            ),
            column(width = 3,
                   uiOutput("LCK_Match_team_B")
            ),
            column(width = 6,
                   uiOutput("LCK_Match_picker")
            ),
            column(width = 12,
                   DT::dataTableOutput("LCK_matches")
            )
          )
        )
)