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

###################################
# Loading Dataset & Preprocessing #
###################################

data_rating = read.csv('C:/Users/user/Desktop/Recommender_System/Data/df_rating.csv')

mtx_rating = as.matrix(data_rating[, -1])

###########################
# Matrix Factorization CF #
###########################

# matrix factorization

num_user = nrow(mtx_rating)
num_item = ncol(mtx_rating)

k = 5 # num. of latent factors
lambda = 0.1 # weight of penalty
learning_rate = 0.01
epoch = 1e3

R = as.matrix(mtx_rating)
P = matrix(runif(num_user*k), nrow = num_user)
Q = matrix(runif(num_item*k), nrow = num_item)

for (e in seq(epoch)) {
  for (u in seq(num_user)) {
    for (i in seq(num_item)) {
      if (!is.na(R[u, i])) {
        e_ui = R[u, i]-t(Q[i, ])%*%P[u, ] %>% 
          as.vector()
        
        P[u, ] = P[u, ]+learning_rate*(e_ui*Q[i, ]-lambda*P[u, ])
        Q[i, ] = Q[i, ]+learning_rate*(e_ui*P[u, ]-lambda*Q[i, ])
      }
    }
  }
  
  if (e%%1e2 == 0) {
    cat('Epoch:', e, 'MAE:', mean(abs(R-P%*%t(Q)), na.rm = T), '\n')
  }
}

# predicting ratings

mtx_rating_pred = P%*%t(Q)

# evaluation: MAE

mean(abs(mtx_rating_pred-mtx_rating), na.rm = T)
