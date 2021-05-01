##### LEC competition server #####

# Data filtered by event
LEC_filt_event <- reactive(data_LEC %>% filter(event %in% input$LEC_event))

# Flags LEC
flags_LEC <- c(
  "AST.png", "XL.png", "S04.png", "FNC.png", "G2.png",
  "MAD.png", "MSF.png", "RGE.png", "SK.png", "VIT.png"
)

# Define colors for each team
flags_LEC <- data.frame(team = sort(unique(data_LEC$team)),
                        img = flags_LEC,
                        color = c("#e51c20", "#000000", "#014a9c", "#ff5800",
                                  "#f52c18", "#c39233", "#a81e31", "#00253d",
                                  "#cccccc", "#f9e300"),
                        team_simplified = c("AST", "XL", "S04", "FNC", "G2",
                                            "MAD", "MSF", "RGE", "SK", "VIT"))

##### Box with value boxes #####
# Nb of games
output$LEC_games <- renderValueBox({
  valueBox(
    nrow(LEC_filt_event())/12,
    "Games",
    icon = icon("gamepad")
  )
})

# Nb of teams
output$LEC_teams <- renderValueBox({
  valueBox(
    length(unique(LEC_filt_event()$team)),
    "Teams",
    icon = icon("users"),
    color = "lime"
  )
})

##### Table for team/player stats #####
stats_LEC <- reactive({
  LEC_filt_event() %>% 
    group_by(player, team) %>% 
    summarise(
      kills = sum(kills),
      deaths = sum(deaths),
      assists = sum(assists),
      games = n(),
      events = paste(unique(event), collapse = ", ")) %>% 
    mutate(kda = calculate_kda(kills, deaths, assists),
           kills_per_games = round(kills/games, 2),
           assists_per_games = round(assists/games, 2),
           deaths_per_games = round(deaths/games, 2))
})

# Teams stats
stats_LEC_teams <- reactive({
  stats_LEC()[stats_LEC()$player == "",]
})

# Players stats
stats_LEC_players <- reactive({
  stats_LEC()[stats_LEC()$player != "",]
})

##### Team graph #####
# Slider
output$LEC_competition_team_graph_slider <- renderUI({
  sliderInput("LEC_competition_team_graph_slider", "Number of teams displayed",
              value = nrow(stats_LEC_teams()),
              min = min(nrow(stats_LEC_teams()), 3),
              max = nrow(stats_LEC_teams()),
              step = 1)
})

# Graph
output$LEC_competition_team_graph <- renderAmCharts({
  # Getting stat to display
  stat <- paste(unlist(strsplit(tolower(input$LEC_competition_team_graph_choice), " ")), collapse = "_")
  
  # Sorting and adding ranking
  if(stat %in% c("deaths", "deaths_per_games")){
    temp <- merge(stats_LEC_teams(), flags_LEC) %>% 
      arrange(!!as.symbol(stat)) %>% 
      mutate(ranking = rank(!!as.symbol(stat), ties.method = "min"))
  } else {
    temp <- merge(stats_LEC_teams(), flags_LEC) %>% 
      arrange(-!!as.symbol(stat)) %>% 
      mutate(ranking = rank(-!!as.symbol(stat), ties.method = "min"))
  }
  
  # Keeping n first teams
  temp <- temp %>% slice(1:input$LEC_competition_team_graph_slider)
  
  # Changing title in function of number of teams
  if(input$LEC_competition_team_graph_slider == nrow(stats_LEC_teams())){
    title <- paste0(input$LEC_competition_team_graph_choice, ' Ranking (',
                    paste0(input$LEC_event, collapse = ", "), ")")
  } else {
    title <- paste0(input$LEC_competition_team_graph_choice, ' Ranking - Top ',
                    input$LEC_competition_team_graph_slider, ' (',
                    paste0(input$LEC_event, collapse = ", "), ")")
  }
  
  # Building graph
  pipeR::pipeline(
    amSerialChart(categoryField = 'team', creditsPosition = "top-right"),
    setDataProvider(temp),
    addGraph(balloonText = '<b>[[ranking]]. [[category]]: [[value]]</b>\nGames: [[games]]',
             type = 'column', valueField = stat,
             fillAlphas = 1, lineAlpha = 0,
             fillColorsField = 'color',
             labelText = "[[value]]"),
    addValueAxis(minimum = 0),
    addTitle(text = title),
    setExport(enabled = TRUE)
  )
})

##### Player graph #####
# Slider
output$LEC_competition_player_graph_slider <- renderUI({
  sliderInput("LEC_competition_player_graph_slider", "Number of players displayed",
              value = min(20, nrow(stats_LEC_players())),
              min = min(nrow(stats_LEC_players()), 3),
              max = nrow(stats_LEC_players()),
              step = 1)
})

# Graph
output$LEC_competition_player_graph <- renderAmCharts({
  # Getting stat to display
  stat <- paste(unlist(strsplit(tolower(input$LEC_competition_player_graph_choice), " ")), collapse = "_")
  
  # Sorting and adding ranking
  if(stat %in% c("deaths", "deaths_per_games")){
    temp <- merge(stats_LEC_players(), flags_LEC) %>% 
      arrange(!!as.symbol(stat)) %>% 
      mutate(ranking = rank(!!as.symbol(stat), ties.method = "min"))
  } else {
    temp <- merge(stats_LEC_players(), flags_LEC) %>% 
      arrange(-!!as.symbol(stat)) %>% 
      mutate(ranking = rank(-!!as.symbol(stat), ties.method = "min"))
  }
  
  # Keeping n first players
  temp <- temp %>% slice(1:input$LEC_competition_player_graph_slider)
  
  # Changing title in function of number of players
  if(input$LEC_competition_player_graph_slider == nrow(stats_LEC_players())){
    title <- paste0(input$LEC_competition_player_graph_choice, ' Ranking (',
                    paste0(input$LEC_event, collapse = ", "), ")")
  } else {
    title <- paste0(input$LEC_competition_player_graph_choice, ' Ranking - Top ',
                    input$LEC_competition_player_graph_slider,' (',
                    paste0(input$LEC_event, collapse = ", "), ")")
  }
  
  # Building graph
  pipeR::pipeline(
    amSerialChart(categoryField = 'player', creditsPosition = "top-right"),
    setDataProvider(temp),
    addGraph(balloonText = paste0('<b>[[ranking]]. [[team_simplified]] [[category]]: [[value]]</b>\nGames: [[games]]'),
             type = 'column', valueField = stat,
             fillAlphas = 1, lineAlpha = 0,
             fillColorsField = 'color',
             labelText = "[[value]]"),
    addValueAxis(minimum = 0),
    setCategoryAxis(labelRotation = ifelse(nrow(temp) > 20, 45, 0)),
    addTitle(text = title),
    setExport(enabled = TRUE)
  )
})