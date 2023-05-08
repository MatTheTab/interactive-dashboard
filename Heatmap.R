library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(hrbrthemes)

games <- read.csv("steam-games-dataset/clustered_games.csv")

chosen_cluster <- 4

#gameGenres <- c("Indie","Action","Adventure","Casual","Strategy","RPG","Simulation","EarlyAccess","FreeToPlay","Sports","Racing","MassivelyMultiplayer")

logical_to_binary <- function(x){
  return(ifelse(x=="True", 1, 0))
}

cluster_genres <- games %>% select(GenreIsIndie:GenreIsMassivelyMultiplayer) %>% filter(games$cluster == chosen_cluster)
cluster_genres <- cluster_genres %>% lapply(logical_to_binary)
cluster_genres <- data.frame(cluster_genres)

genre_x <- c()
genre_y <- c()
genre_f <- c()
for(x in names(cluster_genres)){
  for(y in names(cluster_genres)){
    f <- 0
    if(x!=y){
      f <- cluster_genres[cluster_genres[x] == 1] %>% sum(unlist(cluster_genres[y]))
    }
    else{
      f <- -1
    }
    genre_x <- append(genre_x, substring(x, 8, 20))
    genre_y <- append(genre_y, substring(y, 8, 20))
    genre_f <- append(genre_f, f)
  }
}

genre_heat <- data.frame(x=genre_x, y=genre_y, z=genre_f)

title_size <- 20
label_size <- 16
tick_size <- 12
grid_size <- 0.5

genre_heat %>% ggplot(aes(x, y, fill = z)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
  theme_ipsum() +
  ggtitle(paste("Co-occurrance of Genres in Cluster",chosen_cluster)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_rect(fill="#2c323b"),
    plot.title=element_text(size=title_size, colour = "white", hjust = 0.2),
    axis.text = element_text(size=tick_size, color = "white"),
    panel.grid = element_line(color="#DDDDDD"),
    panel.background = element_rect(fill="#2c323b"),
    legend.position = "none")

