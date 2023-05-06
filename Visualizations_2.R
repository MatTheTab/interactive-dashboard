library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)

games <- read.csv("C:\\Users\\Komputer\\interactive-dashboard\\steam-games-dataset\\clustered_games.csv")
games$X.1<-NULL
games$X<-NULL
chosen_game="Terraria"

cluster_value <- games %>% filter(QueryName == chosen_game) %>% pull(cluster)
similar_games <- games %>% filter(cluster == cluster_value)
similar_games <- similar_games %>% filter(QueryName != chosen_game)

###################Density Plot for Meta critic scores##########################

# Change it to colors corresponding to game scores
game_score <- games %>%
  filter(QueryName == chosen_game) %>%
  pull(Metacritic)

ggplot(similar_games, aes(x = Metacritic)) +
  stat_density(geom = "line", color = "black", size = 1) +
  stat_density(geom = "area", alpha = .3, fill = "black") +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 70) +
  scale_color_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 70) +
  labs(title = "Distribution of Metascores for Recommended Games", x = "Metascore",
       y = "Density") + 
  geom_vline(aes(xintercept = game_score), color = "red") +
  annotate("text", x=game_score+5, y=0.022, label=chosen_game, angle=90,
           hjust=-0.2, size=4) +
  xlim(0,100)
  


#################Bar plot for distribution of genres############################

