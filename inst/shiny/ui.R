header <- dashboardHeader(title = "League Of Legends")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("home")),
    # LCK
    menuItem("LCK", tabName = "LCK", icon = icon("globe-asia"),
             menuSubItem("Competition", tabName = "LCK_competition", icon = icon("sitemap")),
             menuSubItem("Team", tabName = "LCK_team", icon = icon("users")),
             menuSubItem("Player", tabName = "LCK_player", icon = icon("user"))),
    # LPL
    menuItem("LPL", tabName = "LPL", icon = icon("globe-asia")),
    # LEC
    menuItem("LEC", tabName = "LEC", icon = icon("globe-europe"),
             menuSubItem("Competition", tabName = "LEC_competition", icon = icon("sitemap")),
             menuSubItem("Team", tabName = "LEC_team", icon = icon("users")),
             menuSubItem("Player", tabName = "LEC_player", icon = icon("user")),
             menuSubItem("Matches", tabName = "LEC_match", icon = icon("times"))),
    # LFL
    menuItem("LFL", tabName = "LFL", icon = icon("cheese"),
             menuSubItem("Competition", tabName = "LFL_competition", icon = icon("sitemap")))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Home",
            source(file.path("ui", "Home.R"), local = TRUE)$value
    ),
    # LCK
    tabItem(tabName = "LCK_competition",
            source(file.path("ui", "LCK/LCK_competition.R"), local = TRUE)$value
    ),
    tabItem(tabName = "LCK_team",
            source(file.path("ui", "LCK/LCK_team.R"), local = TRUE)$value
    ),
    tabItem(tabName = "LCK_player",
            source(file.path("ui", "LCK/LCK_player.R"), local = TRUE)$value
    ),
    # LPL
    tabItem(tabName = "LPL",
            source(file.path("ui", "LPL.R"), local = TRUE)$value
    ),
    # LEC
    tabItem(tabName = "LEC_competition",
            source(file.path("ui", "LEC/LEC_competition.R"), local = TRUE)$value
    ),
    tabItem(tabName = "LEC_team",
            source(file.path("ui", "LEC/LEC_team.R"), local = TRUE)$value
    ),
    tabItem(tabName = "LEC_player",
            source(file.path("ui", "LEC/LEC_player.R"), local = TRUE)$value
    ),
    tabItem(tabName = "LEC_match",
            source(file.path("ui", "LEC/LEC_match.R"), local = TRUE)$value
    ),
    # LFL
    tabItem(tabName = "LFL_competition",
            source(file.path("ui", "LFL/LFL_competition.R"), local = TRUE)$value
    )
  )
)

ui <- dashboardPage(header, sidebar, body)