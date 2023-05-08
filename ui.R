library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(DT)
library(shinycssloaders)
library(shinyjs)

games <- read.csv("steam-games-dataset/clustered_games.csv")
gameNames <- games %>% arrange(desc(Metacritic)) %>% select(QueryName)
gameGenres <- c("All","Indie","Action","Adventure","Casual","Strategy","RPG","Simulation","EarlyAccess","FreeToPlay","Sports","Racing","MassivelyMultiplayer")

dashboardPage(
  dashboardHeader(title="Game recommendations", titleWidth = 310),
  
  dashboardSidebar(width = 130,
      sidebarMenu(
        menuItem("Dashboard", tabName="dashboard", icon = icon("gamepad", style="color:#dddddd")),
        menuItem("Visualizations", tabName="visualizations", icon=icon("chart-column", class="fa-solid fa-chart-column", style="color:#dddddd")),
        menuItem("Help", tabName="help", icon = icon("circle-question", class="fa-solid", style="color:#dddddd")),
        menuItem("About", tabName="about", icon = icon("circle-info", style="color:#dddddd"))
      )
  ),
  
  dashboardBody(
    useShinyjs(),
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
                  div(style = "height:235px;",
                      htmlOutput("headerimage") %>% withSpinner(5,color.background = "#2c323b", proxy.height = "200px")
                  )
              ),
              column(width=2,
                     div(style="margin-left:-40px; margin-top:-20px; height:200px;",
                         plotlyOutput("scoregauge")
                     )
              ),
              column(width=6,
                     div(style="margin-top:-40px; margin-left:30px; margin-right:20px;",
                       h3("Games you might like:"),
                       DT::dataTableOutput("cluster_games_table"))
              )
          ),
          fluidRow(
              column(width=5,
                     div(style="margin-top:-25px;",
                       h4("Game description:"),
                       div(style="max-height:85px; overflow-y:scroll;",
                           textOutput("description")
                       ),
                       h4("PC minimum requirements:"),
                       div(style="max-height:85px; overflow-y:scroll;",
                           textOutput("requirements"),
                       ),
                       h4("Supported languages:"),
                       div(style="max-height:85px; overflow-y:scroll;",
                           textOutput("languages")
                       ),
                     )
              ),
              column(width=1,
                     div(style="margin-top:15px; margin-right:0px; margin-left:30px;",
                         actionButton("scatterbutton","",icon=icon("hand-dots")),
                         actionButton("barbutton","",icon=icon("chart-bar")),
                         actionButton("densitybutton","",icon=icon("chart-area")),
                         actionButton("plot4","",icon=icon("chart-bar"))
                      )
              ),
              column(width=6,
                     div(style="margin-top:-50px;",
                       div(style="margin-top:-10px; margin-right:30px;",
                           plotlyOutput("scatter"),
                           plotOutput("bar"),
                           plotOutput("density")
                       )
                     )
              )
          ),
          fluidRow(
               div(style = "margin-left:10px;",
                     img(src = "imgs/PP_logotyp_black.png", height=85, width=510))
             )
      
      ),
      tabItem(tabName = "visualizations",
              h2("Advanced Visualizations"),
              "Insert visualizations we want here!"
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
             h5("For this purpose data about almost 1000 games has been used
                and by using clustering techniques as well as similarity
                calculations, the app allows the user to see similar
                games to the one that was chosen. The games were selected based
                on number of recomendations but also some lesser-known games
                had been chosen to allow users to experince the joy of discovering hidden
                gems!"),
             h4("Data used:"),
             a("https://data.world/craigkelly/steam-game-data", href="https://data.world/craigkelly/steam-game-data")
      )
    )
  )
)