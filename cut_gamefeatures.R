library(dplyr)
games <- read.csv("steam-games-dataset/games-features.csv")

set.seed(1234)
selectedGames <- games %>% 
  filter(Metacritic != 0) %>%
  distinct(QueryName, .keep_all = TRUE) %>%
  arrange(desc(RecommendationCount)) %>%
  head(700) %>%
  rbind(games %>%
          arrange(RecommendationCount) %>% 
          filter(Metacritic != 0) %>%
          distinct(QueryName, .keep_all = TRUE) %>%
          head(1000) %>%
          sample_n(300))


write.csv(selectedGames, "game-features-cut.csv")
