tabItem(tabName = "LFL_match",
        fluidRow(
          box(
            title = "Match",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 6,
                   uiOutput("LFL_Match_picker")
            ),
            column(width = 12,
                   DT::dataTableOutput("LFL_matches")
            )
          )
        )
)