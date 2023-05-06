library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(text2vec)
library(scales)

games <- read.csv("C:\\Users\\Komputer\\Downloads\\games-features.csv")

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

games <-selectedGames
games <- games %>%
  distinct(QueryName, .keep_all = TRUE)
games <- games %>%
  distinct(ResponseName, .keep_all = TRUE)

new_games <- games %>%
  select(QueryName, DetailedDescrip, RequiredAge, Metacritic, ReleaseDate,
         CategorySinglePlayer, CategoryMultiplayer, 
         CategoryMMO, CategoryCoop, CategoryVRSupport, 
         GenreIsNonGame, GenreIsIndie, GenreIsAction, 
         GenreIsAdventure, GenreIsCasual, GenreIsStrategy,
         GenreIsRPG, GenreIsSimulation, GenreIsEarlyAccess,
         GenreIsFreeToPlay, GenreIsSports, GenreIsRacing) %>%
  mutate(Year = format(as.Date(ReleaseDate, "%b %d %Y"), "%Y")) %>%
  select(-ReleaseDate)

#Cleaning-up and normalizing
new_games<-na.omit(new_games)
new_games$Year<-as.numeric(new_games$Year)
new_games<- new_games %>%
  mutate(across(where(is.logical), as.numeric)) %>%
  mutate(across(where(is.numeric), rescale))
new_games <- new_games %>% 
  mutate(across(where(is.character) & !c(QueryName, DetailedDescrip), 
                function(x) as.numeric(x == "True")))

#Tokenizing
it <- itoken(new_games$DetailedDescrip, progressbar = FALSE)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

#Combining Tokens and previous data.frame
dtm_df <- as.data.frame(as.matrix(dtm))
combined_df <- cbind(new_games, dtm_df)
combined_df$DetailedDescrip<-NULL
combined_df$QueryName<-NULL

#Clustering
set.seed(23)
km_result <- kmeans(combined_df, centers = 10, nstart = 10)

#Final Solution
games$cluster <- km_result$cluster

write.csv(games, "C:\\Users\Komputer\\interactive-dashboard\\steam-games-dataset\\clustered_games.csv")
