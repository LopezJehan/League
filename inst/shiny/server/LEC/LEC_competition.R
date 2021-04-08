# Data filtered by event
LEC_filt_event <- reactive(data_LEC %>% filter(event %in% input$LEC_event))

##### Flags LEC
flags_LEC <- c(
  "AST.png",
  "XL.png",
  "S04.png",
  "FNC.png",
  "G2.png",
  "MAD.png",
  "MSF.png",
  "RGE.png",
  "SK.png",
  "VIT.png"
)

flags_LEC <- data.frame(team = sort(unique(data_LEC$team)),
                        img = flags_LEC)

##### First Box #####
output$LEC_games <- renderValueBox({
  valueBox(
    nrow(LEC_filt_event())/12,
    "Games",
    icon = icon("gamepad")
  )
})

output$LEC_teams <- renderValueBox({
  valueBox(
    length(unique(LEC_filt_event()$team)),
    "Teams",
    icon = icon("users"),
    color = "lime"
  )
})