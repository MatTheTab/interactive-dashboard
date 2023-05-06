library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(plotly)

games <- read.csv("steam-games-dataset/game-features-cut.csv")
gameNames <- games %>% arrange(desc(Metacritic)) %>% select(ResponseName)
gameGenres = c("All","Indie","Action","Adventure","Casual","Strategy","RPG","Simulation","EarlyAccess","FreeToPlay","Sports","Racing","MassivelyMultiplayer")

prettyTable <- function(table_df, round_columns_func=is.numeric, round_digits=0) {
  DT::datatable(table_df, style="bootstrap", filter = "top", rownames = FALSE, extensions = "Buttons", 
                options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
    formatRound(unlist(lapply(table_df, round_columns_func)), round_digits)
}

dashboardPage(
  dashboardHeader(title="Game recommendations", titleWidth = 310),
  
  dashboardSidebar(width = 130,
      sidebarMenu(
        menuItem("Dashboard", tabName="dashboard", icon = icon("gamepad", style="color:#dddddd")),
        menuItem("Help", tabName="help", icon = icon("circle-question", class="fa-solid", style="color:#dddddd")),
        menuItem("About", tabName="about", icon = icon("circle-info", style="color:#dddddd"))
      )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    
    tabItems(
      tabItem(tabName="dashboard",
          fluidRow(
              div(style = "margin-left:20px; margin-top:-20px",
                  h2("Select a game")),
              column(width=4,
                  selectizeInput(
                      width="100%",
                      "gamesearch",
                      label = "Game",
                      choices = gameNames,
                      selected = NULL,
                      options = list(
                        placeholder = 'Type the title', maxOptions = 17000)
                      )
              ),
              column(width=3,
                  selectInput(
                      width="100%",
                      "genresearch",
                      label = "Genre",
                      selected = "All",
                      multiple=T,
                      choices = gameGenres,
                  )
              ),
              column(width=5,
                  div(style = "margin-top:-30px;",
                      plotlyOutput("scoregauge")
                  )
              )
              
          ),
          
          fluidRow(
               div(style = "margin-left:10px;",
                     img(src = "imgs/PP_logotyp_black.png", height=85, width=510))
             )
      
      ),
      tabItem(tabName="help",
             h2("Help section"),
             "Information about dashboard usage"
      ),
      tabItem(tabName="about",
             h2("About section"),
             "Created by:", br(),
             "Mateusz Tabaszewski 151945", br(),
             "Barłomiej Pukacki 151942", br(),
             "Data used:", br(),
             a("https://data.world/craigkelly/steam-game-data", href="https://data.world/craigkelly/steam-game-data")
      )
    )
  )
)