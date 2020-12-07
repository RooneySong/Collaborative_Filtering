###########################
# Input Dataframe Example #
###########################

#    User ID Item 1 Item 2 ... Item 4 Item 5
# 1     7345      6     10          7      9
# 2    12431     NA      6         NA      7
# 3    22434      8     10         10     10
# 4    42635      7      9          7      7
# 5    45659      8      8          9     10

###########
# Library #
###########

library(dplyr)

############
# Function #
############

get.cos = function(x, y) {
  is_completed = !is.na(x*y)
  
  x_hat = x[is_completed]
  y_hat = y[is_completed]
  
  return (sum(x_hat*y_hat)/(sqrt(sum(x_hat*x_hat))*sqrt(sum(y_hat*y_hat))))
}

get.pred = function(w, r) {
  is_completed = !is.na(w*r)
  
  w_hat = w[is_completed]
  r_hat = r[is_completed]
  
  return (sum(w_hat*r_hat)/sum(abs(w_hat)))
}

###################################
# Loading Dataset & Preprocessing #
###################################

data_rating = read.csv('C:/Users/user/Desktop/Recommender_System/Data/df_rating.csv')

mtx_rating = as.matrix(data_rating[, -1])

mean_user_rating = mtx_rating %>% 
  apply(1, function(x) mean(x, na.rm = T))

mtx_rating_adj = mtx_rating-mean_user_rating

#################
# User-Based CF #
#################

# creating a user-user matrix by using adjusted cosine similarity

num_user = nrow(mtx_rating)
num_item = ncol(mtx_rating)
mtx_user = matrix(rep(NA, num_user^2), ncol = num_user)

for (i in seq(num_user)) {
  for (j in seq(num_user)) {
    mtx_user[i, j] = get.cos(mtx_rating_adj[i, ], mtx_rating_adj[j, ])
  }
}

# predicting ratings

mtx_rating_pred_1 = matrix(rep(NA, num_user*num_item), ncol = num_item)

for (u in seq(num_user)) {
  for (i in seq(num_item)) {
    mtx_rating_pred_1[u, i] = get.pred(mtx_user[u, ], mtx_rating_adj[, i]) + mean_user_rating[u]
  }
}

# evaluation: MAE

mean(abs((mtx_rating_pred_1-mtx_rating)), na.rm = T)

#################
# Item-Based CF #
#################

# creating a item-item matrix by using adjusted cosine similarity

mtx_item = matrix(rep(NA, num_item^2), ncol = num_item)

for (i in seq(num_item)) {
  for (j in seq(num_item)) {
    mtx_item[i, j] = get.cos(mtx_rating_adj[, i], mtx_rating_adj[, j])
  }
}

# predicting ratings

mtx_rating_pred_2 = matrix(rep(NA, num_user*num_item), ncol = num_item)

for (u in seq(num_user)) {
  for (i in seq(num_item)) {
    mtx_rating_pred_2[u, i] = get.pred(mtx_item[i, ], mtx_rating_adj[u, ]) + mean_user_rating[u]
  }
}

# evaluation: MAE

mean(abs(mtx_rating_pred_2-mtx_rating), na.rm = T)
