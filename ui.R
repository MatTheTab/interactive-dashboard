library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(plotly)

sales <- read.csv("steam-games-dataset/vgsales.csv")
gameNames <- sales %>% arrange(desc(Critic_Score)) %>% select(Name) %>% head(500)
gameGenres <- sales %>% filter(Genre != "") %>% select(Genre) %>% distinct()
gameGenres <- rbind("All", gameGenres)

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
                      "searchbar",
                      label = "Game",
                      choices = gameNames,
                      options = list(
                        placeholder = 'Type the title', maxOptions = 17000)
                      )
              ),
              column(width=2,
                  selectInput(
                      width="100%",
                      "genresearch",
                      label = "Genre",
                      choices = gameGenres,
                  )
              ),
              column(width=6,
                  div(style = "margin-top:-10px;",
                      plotlyOutput("scoregauge")
                  )
              )
              
          ),
          
          fluidRow(
               div(img(src = "imgs/PP_logotyp_black.png", height=85, width=510))
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
             a("https://data.world/mhoangvslev/steam-games-dataset", href="https://data.world/mhoangvslev/steam-games-dataset"), br(),
             a("https://data.world/craigkelly/steam-game-data", href="https://data.world/craigkelly/steam-game-data")
      )
    )
  )
)