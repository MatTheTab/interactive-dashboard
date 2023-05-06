library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)

games <- read.csv("C:\\Users\\Komputer\\interactive-dashboard\\steam-games-dataset\\clustered_games.csv")
games$X.1<-NULL
games$X<-NULL
chosen_game="Terraria"

cluster_value <- games %>% filter(QueryName == chosen_game) %>% pull(cluster)
similar_games <- games %>% filter(cluster == cluster_value)
game_number<-which(similar_games$QueryName == chosen_game)
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

genre_counts <- similar_games %>%
  mutate(across(starts_with("Genre"), ~ .x == "True")) %>%
  summarise(across(starts_with("Genre"), ~ sum(.x))) %>%
  pivot_longer(everything(), names_to = "Genre", values_to = "Count")

temp_data <-similar_games %>% select(starts_with("Genre")) %>% slice(game_number)
temp_data <- temp_data %>% mutate(across(everything(), ~ ifelse(.x, "green", "grey")))
temp_data <- temp_data %>% gather(key = "Genre", value = "Color")
genre_counts <- left_join(genre_counts, temp_data, by="Genre")

genre_counts <- genre_counts %>%
  filter(Genre != "GenreIsAll")

genre_counts <- genre_counts %>%
  arrange(desc(Count))

genre_counts %>%
  mutate(Genre = fct_reorder(Genre, Count)) %>%
  ggplot(aes(x = Count, y = Genre, fill=Color)) +
  geom_col() +
  labs(title = "Histogram of Genre Counts", x = "Genre", y = "Count")

