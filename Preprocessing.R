###########
# Library #
###########

library(dplyr)
library(tidyr)

###################################
# Loading Dataset & Preprocessing #
###################################

data_anime = read.csv('C:/Users/user/Desktop/Recommender_System/Data/anime.csv')
data_rating = read.csv('C:/Users/user/Desktop/Recommender_System/Data/rating.csv') %>% 
  filter(rating != -1)

top10_anime_id = data_anime %>% 
  arrange(-members) %>% 
  head(10) %>% 
  select(anime_id) %>% 
  unlist()

top10_user_id = data_rating %>% 
  group_by(user_id) %>% 
  count() %>% 
  arrange(-n) %>% 
  head(10) %>% 
  select(user_id) %>% 
  unlist()

df_rating = data_rating %>% 
  filter(anime_id %in% top10_anime_id, user_id %in% top10_user_id) %>% 
  spread(anime_id, rating)

write.csv(df_rating, 'df_rating.csv', row.names = F)
