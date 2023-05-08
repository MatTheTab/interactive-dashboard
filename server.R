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

# paste('<img src="',findimage("Counter-Strike: Global Offensive"),'">', sep='', collapse=NULL)


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
    
    observeEvent(input$cluster_games_table_cell_clicked, {
      selected$game = req(input$cluster_games_table_cell_clicked$value)
      updateSelectizeInput(session, inputId = "gamesearch", selected=selected$game)
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
      
      title_size <- 20
      label_size <- 16
      tick_size <- 12
      grid_size <- 0.5
      
      p <- ggplot(head(similar_games,number_of_points), aes(x = RecommendationCount, y = Metacritic,
                                                            text = paste("Title:", QueryName))) +
        geom_point(color="white") +
        labs(x = "RecommendationCount", y = "MetacriticScore") +
        ggtitle("Quality vs Popularity for Similar Games") +
        theme(plot.background = element_rect(fill="#2c323b"),
              plot.title=element_text(size=title_size-1, colour = "white", hjust = 0),
              axis.title.x = element_text(size=label_size, colour = "white"),
              axis.title.y = element_text(size=label_size, colour = "white"),
              axis.text = element_text(size=tick_size, color = "white"),
              panel.background = element_rect(fill="#2c323b"))
      p
      
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
                             decreasing = TRUE)[1]
      
      # Replace Inf values with the second largest non-Inf value times 2
      similar_games$similarity[similar_games$similarity == Inf] <- second_largest * 2
      
      if(is.na(similar_games$similarity[1])){
        similar_games$similarity<-1
      }
      
      x_breaks <- seq(floor(min(similar_games$similarity)),
                      ceiling(max(similar_games$similarity)), length.out = 5)
      
      # Create a horizontal bar plot
      title_size <- 20
      label_size <- 16
      tick_size <- 12
      grid_size <- 0.5
      
      g <- ggplot(head(similar_games,number_of_bars),
                  aes(x = similarity, y = reorder(QueryName, similarity))) +
        geom_bar(stat = "identity", fill="#66c0f4") +
        scale_x_continuous(breaks = x_breaks) +
        labs(x = "Similarity", y = "Game Name") +
        ggtitle("15 Most Similar Games") +
        theme(plot.background = element_rect(fill="#2c323b"),
              plot.title=element_text(size=title_size, colour = "white", hjust = 0),
              axis.title.x = element_text(size=label_size, colour = "white", hjust=0.2),
              axis.title.y = element_text(size=label_size, colour = "white"),
              axis.text = element_text(size=tick_size, color = "white"),
              panel.grid = element_line(color="#DDDDDD"),
              panel.background = element_rect(fill="#2c323b"))
      g
      
    })
    
    output$density <- renderPlot({
      cluster_value <- games %>% filter(QueryName == selected$game) %>% pull(cluster)
      similar_games <- games %>% filter(cluster == cluster_value)
      similar_games <- similar_games %>% filter(QueryName != selected$game)
      
      game_score <- games %>%
        filter(QueryName == selected$game) %>%
        pull(Metacritic)
      
      title_size <- 20
      label_size <- 16
      tick_size <- 12
      grid_size <- 0.5
      
      score_density <- density(similar_games$Metacritic, n = 2^12, kernel="gaussian", bw="nrd0", window = "gaussian")
      similar_scores_density <- data.frame(x=score_density$x, y=score_density$y)
      
      similar_scores_density %>% ggplot(aes(x, y)) + 
        geom_line() + 
        geom_segment(aes(xend=x, yend=0, colour=x)) +
        scale_color_stepsn(colors=c("#FF0000", "#FFCC33", "#66CC33"), breaks=c(50, 75))  +
        ggtitle("Density distribution of scores of similar games") + 
        geom_vline(aes(xintercept = game_score), color = "white", size=1) +
        annotate("text", x=game_score+5, y=0.022, label=selected$game, angle=90, color="white", hjust=0, size=4) +
        xlim(0,100) +
        labs(x = "Metacritic score", color = "Score") +
        theme(plot.background = element_rect(fill="#2c323b"),
              plot.title=element_text(size=title_size, colour = "white", hjust=0.4),
              axis.title.x = element_text(size=label_size, colour = "white"),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size=tick_size, color = "white"),
              axis.text.y = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.background = element_rect(fill="#2c323b"),
              legend.position = "none")
    })
    
  }
)
