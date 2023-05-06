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
  gameCluster <- games[games$QueryName==gamename,]$cluster
  games %>% filter(cluster==gameCluster & QueryName != gamename) %>% 
    select(QueryName, Metacritic, PriceFinal) %>% 
    rename(Game=QueryName, PriceUSD=PriceFinal)
}

# test <- findcluster("Counter-Strike: Global Offensive")

findscore <- function(gamename){
  games[games$QueryName==gamename,]$Metacritic
}

# findscore("Dota 2"))

findimage <- function(gamename){
  games[games$QueryName==gamename,]$HeaderImage
}
paste('<img src="',findimage("Counter-Strike: Global Offensive"),'">', sep='', collapse=NULL)


findchoices <- function(genres){
  filteredGames <- games
  for (genre in genres){
    colName <- paste("GenreIs", genre, sep="")
    filteredGames <- filteredGames %>% filter(get(colName)=="True")
  }
  
  filteredGames %>% select(QueryName)
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


similarity_only_genres <-function(df, row1, row2){
  dist <- 0
  for (col in colnames(df)) {
    if (startsWith(col, "Genre") && is.character(df[[col]]) && all(df[[col]] %in% c("True", "False"))) {
      if (df[row1, col] != df[row2, col]) {
        dist <- dist + 1
      }
    }
  }
  
  return(1/sqrt(dist))
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
            list(range = c(0, 50), color = "#FF0000"),
            list(range = c(51, 75), color="#FFCC33"),
            list(range = c(76, 100), color = "#66CC33"))
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
    
    output$headerimage <- renderText({
        paste('<img src="',findimage(selected$game),'">', sep="")
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
    
    output$bar <- renderPlot({
      number_of_bars=15
      
      cluster_value <- games %>% filter(QueryName == selected$game) %>% pull(cluster)
      similar_games <- games %>% filter(cluster == cluster_value)
      
      chosen_game_row <- which(similar_games$QueryName == selected$game)
      norm_similar_games<-normalize(similar_games)
      norm_similar_games$QueryID=NULL
      norm_similar_games$ResponseID=NULL
      norm_similar_games[is.na(norm_similar_games)] <- 0
      
      similar_games$similarity <- sapply(1:nrow(norm_similar_games),
                                         function(i) similarity_only_genres(norm_similar_games, 
                                                                            chosen_game_row, i))
      similar_games <- similar_games %>% filter(QueryName != selected$game)
      similar_games <- similar_games %>% top_n(number_of_bars, similarity)
      second_largest <- sort(similar_games$similarity[similar_games$similarity != Inf],
                             decreasing = TRUE)[2]
      
      # Replace Inf values with the second largest non-Inf value times 2
      similar_games$similarity[similar_games$similarity == Inf] <- second_largest * 2
      
      
      x_breaks <- seq(floor(min(similar_games$similarity)),
                      ceiling(max(similar_games$similarity)), length.out = 5)
      
      # Create a horizontal bar plot
      g <- ggplot(head(similar_games,number_of_bars),
                  aes(x = similarity, y = reorder(QueryName, similarity))) +
        geom_bar(stat = "identity") +
        scale_x_continuous(breaks = x_breaks) +
        labs(x = "Similarity", y = "Game Name") +
        ggtitle("Most Similar Games")
      g
    })
    
    output$density <- renderPlot({
      cluster_value <- games %>% filter(QueryName == selected$game) %>% pull(cluster)
      similar_games <- games %>% filter(cluster == cluster_value)
      similar_games <- similar_games %>% filter(QueryName != selected$game)
      
      game_score <- games %>%
        filter(QueryName == selected$game) %>%
        pull(Metacritic)
      
      ggplot(similar_games, aes(x = Metacritic)) +
        stat_density(geom = "line", color = "black", linewidth = 1) +
        stat_density(geom = "area", alpha = .3, fill = "black") +
        scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 70) +
        scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 70) +
        labs(title = "Distribution of Metascores for Recommended Games", x = "Metascore",
             y = "Density") + 
        geom_vline(aes(xintercept = game_score), color = "red") +
        annotate("text", x=game_score+5, y=0.022, label=selected$game, angle=90,
                 hjust=-0.2, size=4) +
        xlim(0,100)
    })
    
  }
)
