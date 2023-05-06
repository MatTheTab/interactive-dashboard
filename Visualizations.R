library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)

games <- read.csv("C:\\Users\\Komputer\\interactive-dashboard\\steam-games-dataset\\clustered_games.csv")

# Setting the game manually for testing purposes
chosen_game="Terraria"








##################################Interactive Scatterplot#######################
number_of_points=100

cluster_value <- games %>% filter(QueryName == chosen_game) %>% pull(cluster)
similar_games <- games %>% filter(cluster == cluster_value)
similar_games <- similar_games %>% filter(QueryName != chosen_game)

p <- ggplot(head(similar_games,number_of_points), aes(x = RecommendationCount, y = Metacritic,
                       text = paste("Title:", QueryName))) +
  geom_point() +
  labs(x = "RecommendationCount", y = "MetacriticScore") +
  ggtitle("Quality vs Popularity for Similar Games")

fig <- ggplotly(p)

fig


###################################Barplots#####################################