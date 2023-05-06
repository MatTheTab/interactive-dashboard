###################################################### Libraries

library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)

###################################################### DATA

games <- read.csv("steam-games-dataset/game-features-cut.csv")
games$GenreIsAll = "True"

###################################################### FUNCTIONS

findscore <- function(gamename){
  games[games$ResponseName==gamename,]$Metacritic
}

# findscore("Dota 2"))

findchoices <- function(genres){
  filteredGames <- games
  for (genre in genres){
    colName <- paste("GenreIs", genre, sep="")
    filteredGames <- filteredGames %>% filter(get(colName)=="True")
  }
  
  filteredGames %>% select(ResponseName)
}

# findchoices(c("Action", "Adventure", "Indie", "FreeToPlay"))

###################################################### SERVER

shinyServer(
  function(input, output, session) {
    selected <- reactiveValues(game=NULL, genres="All")
    
    observeEvent(input$gamesearch, {
      selected$game = input$gamesearch
    })
    
    observeEvent(input$genresearch, {
      selected$genres = input$genresearch 
      updateSelectizeInput(session, inputId="gamesearch", choices=findchoices(selected$genres))
    })
    
    output$scoregauge <- renderPlotly({
      fig <- plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = findscore(selected$game),
        title = list(text = "Critic score", font = list(size = 22, color="#dddddd")),
        gauge = list(
          axis = list(
            range = list(0, 100),
            tickwidth = 1,
            tickcolor = "#cccccc",
            tickfont=list(size=13),
            dtick="10",
            ticklabelstep=1,
            tickangle=0
            ),
          bar = list(color = "white", thickness=0.2),
          threshold = list(
            value=findscore(selected$game),
            thickness=1,
            line = list(width=3, color="white")),
          bgcolor = "#cccccc",
          borderwidth = 0,
          steps = list(
            list(range = c(0, 40), color = "#FF0000"),
            list(range = c(41, 79), color="#FFCC33"),
            list(range = c(80, 100), color = "#66CC33"))
          )
        )
      
      fig <- fig %>%
        layout(
          paper_bgcolor = "#2c323b",
          font = list(color = "#dddddd", family = "Arial", size=50),
          margin = list(r=50, t=70, b=50)
        )
      
      fig
    })
    
    
  }
)
