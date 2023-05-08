library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)

games <- read.csv("C:\\Users\\Komputer\\interactive-dashboard\\steam-games-dataset\\clustered_games.csv")
games$X.1<-NULL
games$X<-NULL
# Setting the game manually for testing purposes
chosen_game="Counter-Strike: Global Offensive"

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

##################################Interactive Scatterplot#######################
number_of_points=100

cluster_value <- games %>% filter(QueryName == chosen_game) %>% pull(cluster)
similar_games <- games %>% filter(cluster == cluster_value)
similar_games <- similar_games %>% filter(QueryName != chosen_game)


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
        panel.background = element_rect(fill="#2c323b")) +
  scale_x_log10()
  
p
fig <- ggplotly(p)

fig


###################################Barplots#####################################
number_of_bars=15

cluster_value <- games %>% filter(QueryName == chosen_game) %>% pull(cluster)
similar_games <- games %>% filter(cluster == cluster_value)

chosen_game_row <- which(similar_games$QueryName == chosen_game)
norm_similar_games<-normalize(similar_games)
norm_similar_games$QueryID=NULL
norm_similar_games$ResponseID=NULL
norm_similar_games[is.na(norm_similar_games)] <- 0

similar_games$similarity <- sapply(1:nrow(norm_similar_games),
                                   function(i) similarity(norm_similar_games, 
                                                          chosen_game_row, i))
similar_games <- similar_games %>% filter(QueryName != chosen_game)
similar_games <- similar_games %>% top_n(number_of_bars, similarity)
second_largest <- sort(similar_games$similarity[similar_games$similarity != Inf],
                       decreasing = TRUE)[2]

# Replace Inf values with the second largest non-Inf value times 2
similar_games$similarity[similar_games$similarity == Inf] <- second_largest * 2

if(is.na(similar_games$similarity[1])){
  similar_games$similarity<-1
}

x_breaks <- seq(floor(min(similar_games$similarity)),
                ceiling(max(similar_games$similarity)), length.out = 5)

# Create a horizontal bar plot
p <- ggplot(head(similar_games,number_of_bars), aes(x = similarity, y = reorder(QueryName, similarity))) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = x_breaks) +
  labs(x = "Similarity", y = "Game Name") +
  ggtitle("Most Similar Games")
p

###############################Bar plot based only on genres####################

number_of_bars=15

cluster_value <- games %>% filter(QueryName == chosen_game) %>% pull(cluster)
similar_games <- games %>% filter(cluster == cluster_value)

chosen_game_row <- which(similar_games$QueryName == chosen_game)
norm_similar_games<-normalize(similar_games)
norm_similar_games$QueryID=NULL
norm_similar_games$ResponseID=NULL
norm_similar_games[is.na(norm_similar_games)] <- 0

similar_games$similarity <- sapply(1:nrow(norm_similar_games),
                                   function(i) similarity_only_genres(norm_similar_games, 
                                                          chosen_game_row, i))
similar_games <- similar_games %>% filter(QueryName != chosen_game)
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
g <- ggplot(head(similar_games,number_of_bars),
            aes(x = similarity, y = reorder(QueryName, similarity))) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = x_breaks) +
  labs(x = "Similarity", y = "Game Name") +
  ggtitle("Most Similar Games") 


#########
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
        plot.title=element_text(size=title_size, colour = "white", hjust = 0.2),
        axis.title.x = element_text(size=label_size, colour = "white", hjust=0.2),
        axis.title.y = element_text(size=label_size, colour = "white"),
        axis.text = element_text(size=tick_size, color = "white"),
        panel.grid = element_line(color="#DDDDDD"),
        panel.background = element_rect(fill="#2c323b"))
g

