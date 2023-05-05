library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(text2vec)
library(scales)

games<-read.csv("C:\\Users\\Komputer\\Downloads\\games-features.csv")

##############################CLUSTERING########################################

#Deleting repeated names
games <- games %>%
  distinct(QueryName, .keep_all = TRUE)

#Selecting top games according to RecommendationCount
games <- games %>%
  arrange(desc(RecommendationCount)) %>%
  slice_head(n = 1000)

#Selecting rows to be taken into consideration when clustering
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
set.seed(43)
km_result <- kmeans(combined_df, centers = 10, nstart = 10)

#Final Solution
new_games$cluster <- km_result$cluster

######################################DISTANCES#################################
#Requires normalized attributes

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
