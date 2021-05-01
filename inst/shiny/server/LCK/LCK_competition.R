##### LCK competition server #####

# Data filtered by event
LCK_filt_event <- reactive(data_LCK %>% filter(event %in% input$LCK_event))

# Flags LCK
flags_LCK <- c(
  "AF.png", "DRX.png", "DK.png", "BRO.png", "GEN.png",
  "HLE.png", "KT.png", "LSB.png", "NS.png", "T1.png"
)

# Define colors for each team
flags_LCK <- data.frame(team = sort(unique(data_LCK$team)),
                        img = flags_LCK,
                        color = c("#00a1ea", "#598dff", "#64cac9", "#01482c",
                                  "#a98b2f", "#f3731e", "#ff171f", "#fdbf29",
                                  "#000000", "#e3012b"),
                        team_simplified = c("AF", "DRX", "DK", "BRO", "GEN",
                                            "HLE", "KT", "LSB", "NS", "T1"))

##### Box with value boxes #####
# Nb of games
output$LCK_games <- renderValueBox({
  valueBox(
    nrow(LCK_filt_event())/12,
    "Games",
    icon = icon("gamepad")
  )
})

# Nb of teams
output$LCK_teams <- renderValueBox({
  valueBox(
    length(unique(LCK_filt_event()$team)),
    "Teams",
    icon = icon("users"),
    color = "lime"
  )
})

##### Table for team/player stats #####
stats_LCK <- reactive({
  LCK_filt_event() %>% 
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
stats_LCK_teams <- reactive({
  stats_LCK()[stats_LCK()$player == "",]
})

# Players stats
stats_LCK_players <- reactive({
  stats_LCK()[stats_LCK()$player != "",]
})

##### Team graph #####
# Slider
output$LCK_competition_team_graph_slider <- renderUI({
  sliderInput("LCK_competition_team_graph_slider", "Number of teams displayed",
              value = nrow(stats_LCK_teams()),
              min = min(nrow(stats_LCK_teams()), 3),
              max = nrow(stats_LCK_teams()),
              step = 1)
})

# Graph
output$LCK_competition_team_graph <- renderAmCharts({
  # Getting stat to display
  stat <- paste(unlist(strsplit(tolower(input$LCK_competition_team_graph_choice), " ")), collapse = "_")
  
  # Sorting and adding ranking
  if(stat %in% c("deaths", "deaths_per_games")){
    temp <- merge(stats_LCK_teams(), flags_LCK) %>% 
      arrange(!!as.symbol(stat)) %>% 
      mutate(ranking = rank(!!as.symbol(stat), ties.method = "min"))
  } else {
    temp <- merge(stats_LCK_teams(), flags_LCK) %>% 
      arrange(-!!as.symbol(stat)) %>% 
      mutate(ranking = rank(-!!as.symbol(stat), ties.method = "min"))
  }
  
  # Keeping n first teams
  temp <- temp %>% slice(1:input$LCK_competition_team_graph_slider)
  
  # Changing title in function of number of teams
  if(input$LCK_competition_team_graph_slider == nrow(stats_LCK_teams())){
    title <- paste0(input$LCK_competition_team_graph_choice, ' Ranking (',
                    paste0(input$LCK_event, collapse = ", "), ")")
  } else {
    title <- paste0(input$LCK_competition_team_graph_choice, ' Ranking - Top ',
                    input$LCK_competition_team_graph_slider, ' (',
                    paste0(input$LCK_event, collapse = ", "), ")")
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
output$LCK_competition_player_graph_slider <- renderUI({
  sliderInput("LCK_competition_player_graph_slider", "Number of players displayed",
              value = min(20, nrow(stats_LCK_players())),
              min = min(nrow(stats_LCK_players()), 3),
              max = nrow(stats_LCK_players()),
              step = 1)
})

# Graph
output$LCK_competition_player_graph <- renderAmCharts({
  # Getting stat to display
  stat <- paste(unlist(strsplit(tolower(input$LCK_competition_player_graph_choice), " ")), collapse = "_")
  
  # Sorting and adding ranking
  if(stat %in% c("deaths", "deaths_per_games")){
    temp <- merge(stats_LCK_players(), flags_LCK) %>% 
      arrange(!!as.symbol(stat)) %>% 
      mutate(ranking = rank(!!as.symbol(stat), ties.method = "min"))
  } else {
    temp <- merge(stats_LCK_players(), flags_LCK) %>% 
      arrange(-!!as.symbol(stat)) %>% 
      mutate(ranking = rank(-!!as.symbol(stat), ties.method = "min"))
  }
  
  # Keeping n first players
  temp <- temp %>% slice(1:input$LCK_competition_player_graph_slider)
  
  # Changing title in function of number of players
  if(input$LCK_competition_player_graph_slider == nrow(stats_LCK_players())){
    title <- paste0(input$LCK_competition_player_graph_choice, ' Ranking (',
                    paste0(input$LCK_event, collapse = ", "), ")")
  } else {
    title <- paste0(input$LCK_competition_player_graph_choice, ' Ranking - Top ',
                    input$LCK_competition_player_graph_slider,' (',
                    paste0(input$LCK_event, collapse = ", "), ")")
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