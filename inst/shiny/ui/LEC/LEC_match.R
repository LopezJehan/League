tabItem(tabName = "LEC_match",
        fluidRow(
          box(
            title = "Match",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 6,
                   uiOutput("LEC_Match_picker")
            ),
            column(width = 12,
                   DT::dataTableOutput("LEC_matches")
            )
          )
        )
)