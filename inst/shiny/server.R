server <- function(input, output) {
  source(file.path("server", "LCK.R"), local = TRUE)$value
  source(file.path("server", "LPL.R"), local = TRUE)$value
  # LEC
  source(file.path("server", "LEC/LEC_competition.R"), local = TRUE)$value
}