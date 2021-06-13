##### LFL competition server #####

# Data filtered by event
LFL_filt_event <- reactive(data_LFL %>% filter(event %in% input$LFL_event))

# Flags LFL
flags_LFL <- c(
  "GO.png", "GW.png", "IZI.png", "KC.png", "LDLC.png",
  "MSFP.png", "SLY.png", "BDS.png", "MCES.png", "VITB.png"
)

# Define colors for each team
flags_LFL <- data.frame(team = sort(unique(data_LFL$team)),
                        img = flags_LFL,
                        color = c("#ff0000", "#e25a66", "#00c6ff", "#00379d",
                                  "#00abff", "#a81e31", "#ffc000", "#ea3699",
                                  "#00baff", "#f9e300"),
                        team_simplified = c("GO", "GW", "IZI", "KC", "LDLC",
                                            "MSFP", "SLY", "BDS", "MCES", "VITB"))

##### Box with value boxes #####
# Nb of games
output$LFL_games <- renderValueBox({
  valueBox(
    nrow(LFL_filt_event())/12,
    "Games",
    icon = icon("gamepad")
  )
})

# Nb of teams
output$LFL_teams <- renderValueBox({
  valueBox(
    length(unique(LFL_filt_event()$team)),
    "Teams",
    icon = icon("users"),
    color = "lime"
  )
})

##### Table for team/player stats #####
stats_LFL <- reactive({
  LFL_filt_event() %>% 
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
stats_LFL_teams <- reactive({
  stats_LFL()[stats_LFL()$player == "",]
})

# Players stats
stats_LFL_players <- reactive({
  stats_LFL()[stats_LFL()$player != "",]
})

##### Team graph #####
# Slider
output$LFL_competition_team_graph_slider <- renderUI({
  sliderInput("LFL_competition_team_graph_slider", "Number of teams displayed",
              value = nrow(stats_LFL_teams()),
              min = min(nrow(stats_LFL_teams()), 3),
              max = nrow(stats_LFL_teams()),
              step = 1)
})

# Graph
output$LFL_competition_team_graph <- renderAmCharts({
  # Getting stat to display
  stat <- paste(unlist(strsplit(tolower(input$LFL_competition_team_graph_choice), " ")), collapse = "_")
  
  # Sorting and adding ranking
  if(stat %in% c("deaths", "deaths_per_games")){
    temp <- merge(stats_LFL_teams(), flags_LFL) %>% 
      arrange(!!as.symbol(stat)) %>% 
      mutate(ranking = rank(!!as.symbol(stat), ties.method = "min"))
  } else {
    temp <- merge(stats_LFL_teams(), flags_LFL) %>% 
      arrange(-!!as.symbol(stat)) %>% 
      mutate(ranking = rank(-!!as.symbol(stat), ties.method = "min"))
  }
  
  # Keeping n first teams
  temp <- temp %>% slice(1:input$LFL_competition_team_graph_slider)
  
  # Changing title in function of number of teams
  if(input$LFL_competition_team_graph_slider == nrow(stats_LFL_teams())){
    title <- paste0(input$LFL_competition_team_graph_choice, ' Ranking (',
                    paste0(input$LFL_event, collapse = ", "), ")")
  } else {
    title <- paste0(input$LFL_competition_team_graph_choice, ' Ranking - Top ',
                    input$LFL_competition_team_graph_slider, ' (',
                    paste0(input$LFL_event, collapse = ", "), ")")
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
output$LFL_competition_player_graph_slider <- renderUI({
  sliderInput("LFL_competition_player_graph_slider", "Number of players displayed",
              value = min(20, nrow(stats_LFL_players())),
              min = min(nrow(stats_LFL_players()), 3),
              max = nrow(stats_LFL_players()),
              step = 1)
})

# Graph
output$LFL_competition_player_graph <- renderAmCharts({
  # Getting stat to display
  stat <- paste(unlist(strsplit(tolower(input$LFL_competition_player_graph_choice), " ")), collapse = "_")
  
  # Sorting and adding ranking
  if(stat %in% c("deaths", "deaths_per_games")){
    temp <- merge(stats_LFL_players(), flags_LFL) %>% 
      arrange(!!as.symbol(stat)) %>% 
      mutate(ranking = rank(!!as.symbol(stat), ties.method = "min"))
  } else {
    temp <- merge(stats_LFL_players(), flags_LFL) %>% 
      arrange(-!!as.symbol(stat)) %>% 
      mutate(ranking = rank(-!!as.symbol(stat), ties.method = "min"))
  }
  
  # Keeping n first players
  temp <- temp %>% slice(1:input$LFL_competition_player_graph_slider)
  
  # Changing title in function of number of players
  if(input$LFL_competition_player_graph_slider == nrow(stats_LFL_players())){
    title <- paste0(input$LFL_competition_player_graph_choice, ' Ranking (',
                    paste0(input$LFL_event, collapse = ", "), ")")
  } else {
    title <- paste0(input$LFL_competition_player_graph_choice, ' Ranking - Top ',
                    input$LFL_competition_player_graph_slider,' (',
                    paste0(input$LFL_event, collapse = ", "), ")")
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