library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(chorddiag)

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
                      htmlOutput("headerimage") %>% withSpinner(5, color.background = "#2c323b", proxy.height = "200px")
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
                         actionButton("histbutton","",icon=icon("signal", class="fa-flip-horizontal", style="width:14px;")),
                         actionButton("agebutton","",icon=icon("chart-column"))
                      )
              ),
              column(width=6,
                     div(style="margin-top:-50px;",
                       div(style="margin-top:-10px; margin-right:30px;",
                           plotlyOutput("scatter"),
                           plotOutput("bar"),
                           plotOutput("density"),
                           plotOutput("histogram"),
                           div(style="margin-top:-80px;",
                             checkboxInput("nozeroage", "Omit age 0", value = F),
                             plotOutput("agebars")
                           ),
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
              fluidRow(
                column(width=5,
                       sliderInput("heatmapcluster", "Choose a cluster", 1, 10, value = 1, step=1, width = "100%", animate = T),
                       plotOutput("heatmap")
                ),
                column(width=7,
                       div(style="margin-top:-40px; margin-left:-40px;",
                         chorddiagOutput("chorddiag", width="700px", height="700px"))
                ),
              )
      ),
      tabItem(tabName="help",
             h2("Help section"),
             h4("How to Use the Dashboard:"),
             h5("This section is meant to help the user understand and 
             use this dashboard. All explonations below will refer to the 
             elements presented on the screen with accordance to the numerical representation
             shown below:"),
             h5("insert image here"),
             h5("0 - You can use this element to hide the sidebar"),
             h5("1.A - You can use this field to look up the name of 
                a game you like and the dashboard will show you 
                similar games that you may be interested in. You can either pick
                a game from the list or type the name manually."),
             h5("1.B - You can restrict what genres you are interested in, 
                so that it is easier to find the games you may like
                in the 1.A field."),
             h5("2 - Visualization elements meant to show the user information about
                the picked game as well as similar games that may end up being
                interesting. For more information go to the 'About' section present
                in the panel number 3."),
             h5("2.A - Table with games that are similar to the one you selected
                games in here can be sorted by price and Metacritic score."),
             h5("2.B - Icons allowing the user to switch between 4 visualizations,
                interactive scatterplot-you can see the name of the game by hovering over
                the corresponding point, bar chart showing similar games,
                distribution plot of Metacritic scores for similar games, 
                bar chart showing what age requirements categories apply
                to the similar games. For more information go to the 'About'
                section on the sidebar- element 3."),
             h5("3 - Sidebar with icons allowing for switching between
                different dashboard pages."),
             h5("Dashboard - Displays information about the selected game - starting
                page."),
             h5("Visualizations - Displays general visualizations independent of
                the selected game."),
             h5("Help - Currently visited page, explains how to use the application."),
             h5("About - Gives more detailed description of presented visualizations,
                how similar games are determined, also includes data about authors
                and dataset.")
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
             a("https://data.world/craigkelly/steam-game-data", href="https://data.world/craigkelly/steam-game-data"),
             h4("Info and Graph Descriptions:"),
             h5("The bellow section is an in-detail explanation of visualizations
                presented in this dashboard. The elements will be described consistently
                with what is presented on the image below:"),
             h5("Here insert screenshot of the dashboard"),
             h5("1.A - Text field form which games can be chosen, game can be selected
                from a list or typed-in manually."),
             h5("1.B - Text field from which game genres can be selected, doing so will
                restrict selection of games in 1.A only to the specified genres."),
             h5("2.A - Header image for the selected game"),
             h5("2.B - Description of the chosen game"),
             h5("2.C - Minimum requirements for the game"),
             h5("2.D - Languages supported by the currently selected game"),
             h5("3 - Gauge showing the Metacritic score of the currently selected game
                on the range from 0 to 100, colors on the gauge correspond to appropriate
                colors assigned to games by Metacritic(red- unfavorable review, yellow-
                mixed review and green- favourable review)"),
             h5("4 - Recommended games based on previously performed clustering with
                10 created clusters performed with kmeans method. When clustering 
                game genres, price, year of production as well as description was taken
                into account to create 'similar games' that might interest the user"),
             h5("5 - Panel of possible visualizations"),
             h5("5.A - Scatterplot comparing the Metacritic score as well as 
                number of recommendations for games from the same cluster as selected one-
                'similar games'. This is meant to help users find interesting, underrated
                games. Due to some games being much more popular than others and for the
                sake of clarity, logarithmic scale was applied for the x-axis."),
             h5("5.B - Plot showing the most similar games to the chosen one from the same cluster
                based on genres, the more genres match, the higher the similarity.
                In practice similarity in this case is inversly proportional to 
                Hamming distance calculated on genres, with additional safe-guard against infinite
                similarity in case of 0 distance.
                For some games all genres for all top 15 most similar games may be the same,
                maning that their level of similarity will be equal."),
             h5("5.C - Density distribution of Metacritic scores from the similar games to the 
                one selected, with the currently selected one also marked as a vertical
                line."),
             h5("5.D - Bar chart showing the number of games with the given 
                age requirement for all games vs for the selected cluster."),
             h5("Here insert image from 'Visualizations' tab"),
             h5("Describe in detail this tab")
      )
    )
  )
)