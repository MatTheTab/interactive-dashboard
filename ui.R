library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(DT)
library(shinycssloaders)

games <- read.csv("steam-games-dataset/clustered_games.csv")
gameNames <- games %>% arrange(desc(Metacritic)) %>% select(QueryName)
gameGenres = c("All","Indie","Action","Adventure","Casual","Strategy","RPG","Simulation","EarlyAccess","FreeToPlay","Sports","Racing","MassivelyMultiplayer")

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
                      selected = "Counter-Strike: Global Offensive",
                      options = list(
                        placeholder = 'Type the title', maxOptions = 17000)
                      ),
                  selectInput(
                    width="100%",
                    "genresearch",
                    label = "Genre",
                    selected = "All",
                    multiple=T,
                    choices = gameGenres,
                  ),
                  div(style = "margin-top:-20px; height:200px;",
                      plotlyOutput("scoregauge")
                  )
              ),
              column(width=7, offset=1,
                     htmlOutput("headerimage")  %>% withSpinner()
                  
              )
          ),
          fluidRow(
              column(width=7,
                  plotlyOutput("scatter")
                     
              ),
              column(width=5,
                  h3("Games you might like:"),
                  DT::dataTableOutput("cluster_games_table")
              )
          ),
          fluidRow(
              column(width=6,
                  plotOutput("bar")
              ),
              column(width=6,
                  plotOutput("density")
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
             
             h4("Created by:"),
             "Mateusz Tabaszewski 151945", br(),
             "Barłomiej Pukacki 151942", br(),
             h4("Description:"),
             h5("Video games are the biggest modern medium for
                entertainment, with over 10 000 games releasing yearly,
                it has become nigh impossible to find the game that is
                just right for you."),
             h5("Fortunately our application functions
                as a recommendation system for all those seeking
                new and exciting games to play. Choose a game you already 
                know and like, and see what other exciting gaming adventures
                may be worth trying out!"),
             h4("Data used:"),
             a("https://data.world/craigkelly/steam-game-data", href="https://data.world/craigkelly/steam-game-data")
      )
    )
  )
)