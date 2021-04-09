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
                        img = flags_LEC,
                        color = c("#e51c20", "#000000", "#014a9c",
                                  "#ff5800", "#f52c18", "#c39233",
                                  "#a81e31", "#00253d", "#cccccc",
                                  "#f9e300"))

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

##### Chart #####
stats_LEC <- reactive({
  LEC_filt_event() %>% 
    group_by(player, team) %>% 
    summarise(
      kills = sum(kills),
      deaths = sum(deaths),
      assists = sum(assists),
      games = n()) %>% 
    mutate(kda = calculate_kda(kills, deaths, assists))
})

stats_LEC_teams <- reactive({
  stats_LEC()[stats_LEC()$player == "",]
})

stats_LEC_players <- reactive({
  stats_LEC()[stats_LEC()$player != "",]
})

output$LEC_competition_team_graph <- renderAmCharts({
  temp <<- merge(stats_LEC_teams(), flags_LEC) %>% 
    arrange(-kda)
  
  pipeR::pipeline(
    amSerialChart(categoryField = 'team'),
    setDataProvider(temp),
    # addGraph(balloonText = '<img src=[[img]]></img><b>[[category]]: [[value]]</b>',
    #          type = 'column', valueField = 'kda',
    #          fillAlphas = 1, lineAlpha = 0,
    #          labelText = "[[value]]"),
    addGraph(balloonText = '<b>[[category]]: [[value]]</b>',
             type = 'column', valueField = 'kda',
             fillAlphas = 1, lineAlpha = 0,
             fillColorsField = 'color',
             labelText = "[[value]]"),
    addValueAxis(minimum = 0),
    addTitle(text = 'KDA Ranking'),
    setExport(enabled = TRUE)
  )
  # amBarplot(x = "team", y = "kda", data = temp)
})

output$LEC_competition_player_graph <- renderAmCharts({
  temp <- merge(stats_LEC_players(), flags_LEC) %>% 
    arrange(-kda)
  
  pipeR::pipeline(
    amSerialChart(categoryField = 'player'),
    setDataProvider(temp),
    addGraph(balloonText = '<b>[[category]]: [[value]]</b>',
             type = 'column', valueField = 'kda',
             fillAlphas = 1, lineAlpha = 0,
             fillColorsField = 'color',
             labelText = "[[value]]"),
    addValueAxis(minimum = 0),
    addTitle(text = 'KDA Ranking'),
    setExport(enabled = TRUE)
  )
  
  # amBarplot(x = "player", y = "kda", data = temp)
})