library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(DT)
library(ggplot2)
library(tidyr)

###################################################### Data

games <- read.csv("steam-games-dataset/clustered_games.csv")

###################################################### Functions

findcluster <- function(gamename){
  gameCluster <- games[games$ResponseName==gamename,]$cluster
  games %>% filter(cluster==gameCluster & ResponseName != gamename) %>% select(ResponseName, Metacritic) %>% rename(Game=ResponseName)
}

# test <- findcluster("Counter-Strike: Global Offensive")

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

prettyTable <- function(table_df, round_columns_func=is.numeric, round_digits=0) {
  DT::datatable(table_df, style="jqueryui", rownames = F, selection="single",
                options = list(dom = 'tp', scrollY="250px",  pageLength=100))
}

normalize <- function(df) {
  df_norm <- df
  df_norm <- df_norm %>%
    mutate_if(is.numeric, function(x) (x - min(x)) / (max(x) - min(x)))
  
  return(df_norm)
}

similarity <- function(df, row1, row2) {
  row1_subset <- df[row1, ] %>% select_if(is.numeric)
  row2_subset <- df[row2, ] %>% select_if(is.numeric)
  distance <- sum((row1_subset - row2_subset)^2)
  
  for (col in colnames(df)) {
    # Check if the column is a character column with "True" or "False" values
    if (is.character(df[[col]]) && all(df[[col]] %in% c("True", "False"))) {
      # Check if the values in the two rows match
      if (df[row1, col] != df[row2, col]) {
        # If the values do not match, add 1 to the distance variable
        distance <- distance + 1
      }
    }
  }
  
  return(1/sqrt(distance))
}

###################################################### Server

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
        width=250,
        height=150,
        value = findscore(selected$game),
        title = list(text = "Metacritic score", font = list(size = 22, color="#dddddd")),
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
          margin = list(r=40, l=40, t=70, b=10)
        )
      
      fig
    })
    
    output$cluster_games_table <- DT::renderDataTable({
      prettyTable(findcluster(selected$game))
    })
    
    output$scatter <- renderPlotly({
      number_of_points=100
      
      cluster_value <- games %>% filter(QueryName == selected$game) %>% pull(cluster)
      similar_games <- games %>% filter(cluster == cluster_value)
      similar_games <- similar_games %>% filter(QueryName != selected$game)
      
      p <- ggplot(head(similar_games,number_of_points), aes(x = RecommendationCount, y = Metacritic,
                                                            text = paste("Title:", QueryName))) +
        geom_point() +
        labs(x = "RecommendationCount", y = "MetacriticScore") +
        ggtitle("Quality vs Popularity for Similar Games")
      
      fig <- ggplotly(p)
      fig
    })
    
    # output$bar <- renderPlot({
    #   
    #   number_of_bars=15
    #   
    #   cluster_value <- games %>% filter(QueryName == selected$game) %>% pull(cluster)
    #   similar_games <- games %>% filter(cluster == cluster_value)
    #   
    #   chosen_game_row <- which(similar_games$QueryName == selected$game)
    #   norm_similar_games<-normalize(similar_games)
    #   norm_similar_games$QueryID=NULL
    #   norm_similar_games$ResponseID=NULL
    #   norm_similar_games[is.na(norm_similar_games)] <- 0
    #   
    #   similar_games$similarity <- sapply(1:nrow(norm_similar_games),
    #                                      function(i) similarity(norm_similar_games, 
    #                                                             chosen_game_row, i))
    #   similar_games <- similar_games %>% filter(QueryName != selected$game)
    #   similar_games <- similar_games %>% top_n(number_of_bars, similarity)
    #   x_breaks <- seq(floor(min(similar_games$similarity)),
    #                   ceiling(max(similar_games$similarity)), length.out = 5)
    #   
    #   # Create a horizontal bar plot
    #   p <- ggplot(similar_games, aes(x = similarity, y = reorder(QueryName, similarity))) +
    #     geom_bar(stat = "identity") +
    #     scale_x_continuous(breaks = x_breaks) +
    #     labs(x = "Similarity", y = "Game Name")
    #   p
    # })
    
  }
)
