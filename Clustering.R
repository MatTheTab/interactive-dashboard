library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(text2vec)

games<-read.csv("C:\\Users\\Komputer\\Downloads\\games-features.csv")

new_games <- games %>%
  select(QueryName, DetailedDescrip, RequiredAge, Metacritic, ReleaseDate) %>%
  mutate(Year = format(as.Date(ReleaseDate, "%b %d %Y"), "%Y")) %>%
  select(-ReleaseDate)

new_games<-na.omit(new_games)

new_games<-head(new_games,1000)

it <- itoken(new_games$DetailedDescrip, progressbar = FALSE)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

dtm_df <- as.data.frame(as.matrix(dtm))
combined_df <- cbind(new_games, dtm_df)
combined_df$DetailedDescrip<-NULL
combined_df$QueryName<-NULL

set.seed(123)
km_result <- kmeans(combined_df, centers = 3, nstart = 25)

# View cluster assignments
km_result$cluster
combined_df$cluster <- km_result$cluster

combined_df <- combined_df %>%
  select(RequiredAge, Metacritic, cluster)

