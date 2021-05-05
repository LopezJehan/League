server <- function(input, output) {
  # LCK
  source(file.path("server", "LCK/LCK_competition.R"), local = TRUE)$value
  source(file.path("server", "LCK/LCK_team.R"), local = TRUE)$value
  source(file.path("server", "LCK/LCK_player.R"), local = TRUE)$value
  # LPL
  source(file.path("server", "LPL.R"), local = TRUE)$value
  # LEC
  source(file.path("server", "LEC/LEC_competition.R"), local = TRUE)$value
  source(file.path("server", "LEC/LEC_team.R"), local = TRUE)$value
  source(file.path("server", "LEC/LEC_player.R"), local = TRUE)$value
  source(file.path("server", "LEC/LEC_match.R"), local = TRUE)$value
}