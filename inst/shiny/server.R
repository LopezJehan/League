server <- function(input, output) {
  source(file.path("server", "LCK.R"), local = TRUE)$value
  source(file.path("server", "LPL.R"), local = TRUE)$value
  source(file.path("server", "LEC.R"), local = TRUE)$value
}