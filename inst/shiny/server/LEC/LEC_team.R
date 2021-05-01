##### LEC team server #####

# Team picker input
output$LEC_team <- renderUI({
  shinyWidgets::pickerInput("LEC_team", "Choose a team",
                            choices = sort(unique(LEC_filt_event()$team)),
                            choicesOpt = list(content =  
                                                lapply(sort(unique(LEC_filt_event()$team)), FUN = function(team) {
                                                  HTML(paste(
                                                    tags$img(src=flags_LEC[flags_LEC$team == team,]$img),
                                                    tags$b(team)
                                                  ))
                                                })
                            ))
})

# Data filtered by team
LEC_filt_team <- reactive(LEC_filt_event() %>% filter(team %in% input$LEC_team))

##### Box with value boxes #####
# Nb of games
output$LEC_team_games <- renderValueBox({
  valueBox(
    nrow(LEC_filt_team())/6,
    "Games",
    icon = icon("gamepad")
  )
})

# Nb of wins
output$LEC_team_wins <- renderValueBox({
  valueBox(
    paste0(sum(LEC_filt_team()$result)/6, "-",
           nrow(LEC_filt_team())/6 - sum(LEC_filt_team()$result)/6),
    "Wins-Losses",
    icon = icon("trophy"),
    color = "lime"
  )
})

# Winrate
output$LEC_team_winrate <- renderValueBox({
  valueBox(
    paste0(round(100*sum(LEC_filt_team()$result)/nrow(LEC_filt_team()),1),"%"),
    "Winrate",
    icon = icon("percent"),
    color = "fuchsia"
  )
})

# Chosen team stats for players
stats_LEC_players_team <- reactive({
  stats_LEC_players() %>% filter(team == input$LEC_team)
})

# Chosen team stats for the team
stats_LEC_team_one <- reactive({
  stats_LEC_teams() %>% filter(team == input$LEC_team)
})

##### Player graph from chosen team #####
# Slider
output$LEC_team_player_graph_slider <- renderUI({
  sliderInput("LEC_team_player_graph_slider", "Number of players displayed",
              value = nrow(stats_LEC_players_team()),
              min = min(nrow(stats_LEC_players_team()), 3),
              max = nrow(stats_LEC_players_team()),
              step = 1)
})

# Graph
output$LEC_team_player_graph <- renderAmCharts({
  # Getting stat to display
  stat <- paste(unlist(strsplit(tolower(input$LEC_team_player_graph_choice), " ")), collapse = "_")
  
  # Sorting and adding ranking
  if(stat %in% c("deaths", "deaths_per_games")){
    temp <- merge(stats_LEC_players_team(), flags_LEC) %>% 
      arrange(!!as.symbol(stat)) %>% 
      mutate(ranking = rank(!!as.symbol(stat), ties.method = "min"))
  } else {
    temp <- merge(stats_LEC_players_team(), flags_LEC) %>% 
      arrange(-!!as.symbol(stat)) %>% 
      mutate(ranking = rank(-!!as.symbol(stat), ties.method = "min"))
  }
  
  # Keeping n first players
  temp <- temp %>% slice(1:input$LEC_team_player_graph_slider)
  
  # Changing title in function of number of players
  if(input$LEC_team_player_graph_slider == nrow(stats_LEC_players_team())){
    title <- paste0(input$LEC_team_player_graph_choice, ' Ranking (',
                    stats_LEC_team_one()$events, ')')
  } else {
    title <- paste0(input$LEC_team_player_graph_choice, ' Ranking - Top ',
                    input$LEC_team_player_graph_slider, 
                    ' (', stats_LEC_team_one()$events, ")")
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