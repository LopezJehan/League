tabItem(tabName = "LEC_match",
        fluidRow(
          box(
            title = "Match",
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            column(width = 6,
                   sliderInput("LEC_Match_nb", label = "Match number",
                               min = 1, max = nrow(data_LEC)/12,
                               value = 1, step = 1)
            ),
            column(width = 12,
                   DT::dataTableOutput("LEC_matches")
            )
          )
        )
)