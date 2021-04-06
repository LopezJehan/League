header <- dashboardHeader(title = "League Of Legends")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("LCK", tabName = "LCK", icon = icon("globe-asia")),
    menuItem("LPL", tabName = "LPL", icon = icon("globe-asia")),
    menuItem("LEC", tabName = "LEC", icon = icon("globe-europe"))
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
    tabItem(tabName = "LEC",
            source(file.path("ui", "LEC.R"), local = TRUE)$value
    )
  )
)

ui <- dashboardPage(header, sidebar, body)