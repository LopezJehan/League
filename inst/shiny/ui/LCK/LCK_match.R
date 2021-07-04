tabItem(tabName = "LCK_match",
        fluidRow(
          box(
            title = "Match",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 6,
                   uiOutput("LCK_Match_picker")
            ),
            column(width = 12,
                   DT::dataTableOutput("LCK_matches")
            )
          )
        )
)