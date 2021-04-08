header <- dashboardHeader(title = "League Of Legends")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("LCK", tabName = "LCK", icon = icon("globe-asia")),
    menuItem("LPL", tabName = "LPL", icon = icon("globe-asia")),
    menuItem("LEC", tabName = "LEC", icon = icon("globe-europe"),
             menuSubItem("Competition", tabName = "LEC_competition", icon = icon("sitemap")),
             menuSubItem("Team", tabName = "LEC_team", icon = icon("users")),
             menuSubItem("Player", tabName = "LEC_player", icon = icon("user")))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "LCK",
            source(file.path("ui", "LCK.R"), local = TRUE)$value
    ),
    tabItem(tabName = "LPL",
            source(file.path("ui", "LPL.R"), local = TRUE)$value
    ),
    # LEC
    tabItem(tabName = "LEC_competition",
            source(file.path("ui", "LEC/LEC_competition.R"), local = TRUE)$value
    ),
    tabItem(tabName = "LEC_team",
            source(file.path("ui", "LEC/LEC_team.R"), local = TRUE)$value
    )
  )
)

ui <- dashboardPage(header, sidebar, body)