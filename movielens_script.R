#############################
# Begin: code provided by EdX 
#############################

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

############################
# End: code provided by EdX 
############################

if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

#################################
# Visual inspection of the header
# and basic statistics on data
################################

kable(head(edx[,1:6]))

kable(summary(edx[,1:6]))


#####################################
# Separating release date from title,
# calculating age of movie at rating
# time, isolating hour of rating to
# account for diverse viewers
#####################################

edx <- edx %>%
  extract(title, c("clean_title","year_release"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove=FALSE) %>%
  mutate(year_release=as.integer(year_release)) %>%
  mutate(movie_age = as.integer(year(as.POSIXct(timestamp, origin="1970-01-01"))) - year_release) %>%
  mutate(rate_hour = as.integer(hour(as.POSIXct(timestamp, origin="1970-01-01"))))

validation <- validation %>%
  extract(title, c("clean_title","year_release"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove=FALSE) %>%
  mutate(year_release=as.integer(year_release)) %>%
  mutate(movie_age = as.integer(year(as.POSIXct(timestamp, origin="1970-01-01"))) - as.integer(year_release)) %>%
  mutate(rate_hour = as.integer(hour(as.POSIXct(timestamp, origin="1970-01-01"))))


#################################
# Verfifying coherence of data,
# resetting movie_age for
# negative values due to rounding 
#################################

# Are there NAs?

sum(is.na(edx))
sum(is.na(validation))

# Are there outliers given the respective type and rage of data?

summary(data.frame(edx$rating,edx$year_release,edx$movie_age,edx$rate_hour))
summary(data.frame(validation$rating,validation$year_release,validation$movie_age,validation$rate_hour))

# Some instances of movie_age are negative, due to rounding (movie watched on the same year it was released)

sum(edx$movie_age <0)
sum(validation$movie_age <0)

# Resetting these negative numbers to zero

edx$movie_age[edx$movie_age <0] = 0
validation$movie_age[validation$movie_age <0] = 0

# Checking that the changes were taken into account

summary(data.frame(edx$rating,edx$year_release,edx$movie_age,edx$rate_hour))
summary(data.frame(validation$rating,validation$year_release,validation$movie_age,validation$rate_hour))


##########################
# Dropping useless columns,
# renamming title column
##########################

edx <- edx %>%
  select(-c(timestamp,title)) %>%
  rename(title = clean_title) 

validation <- validation %>%
  select(-c(timestamp,title)) %>%
  rename(title = clean_title)


###########################
# Plotting the key data
###########################

# Number of ratings per movie

edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins=50, color="black") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Number of ratings per movie")

# Number of ratings per user

edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins=50, color="black") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of users") +
  ggtitle("Number of ratings per user")

# Mean rating per user

edx %>%
  group_by(userId) %>%
  summarize(m_r = mean(rating)) %>%
  ggplot(aes(m_r)) +
  geom_histogram(bins=50, color="black") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean rating per user")

# Mean rating per user (who rated at least 100 movies)

edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(m_r = mean(rating)) %>%
  ggplot(aes(m_r)) +
  geom_histogram(bins=50, color="black") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean rating per user (who rated at least 100 movies)")

# Mean rating by movie age at time of rating

edx %>%
  group_by(movie_age) %>%
  summarize(m_r = mean(rating)) %>%
  ggplot(aes(m_r,movie_age)) +
  geom_point(color="black") +
  xlab("Mean rating") +
  ylab("Movie age") +
  ggtitle("Mean rating by movie age")

# Mean rating by hour of rating

edx %>%
  group_by(rate_hour) %>%
  summarize(m_r = mean(rating)) %>%
  ggplot(aes(m_r,rate_hour)) +
  geom_point(color="black") +
  xlab("Mean rating") +
  ylab("Hour of rating") +
  ggtitle("Mean rating by hour of rating")

##############################
# Computation of loss function
##############################

RMSE <- function(true_rating, pred_rating){
  sqrt(mean((true_rating - pred_rating)^2))
}

#############################
# Naive, average rating model
#############################

mu <- mean(edx$rating)

rmse_naive <- data.frame(method = "Naive, average rating model", RMSE = RMSE(validation$rating,mu))

kable(rmse_naive)


###############################################
# Accounting for movie effect: some movies tend
# to be rated systematically lower than others 
###############################################

# b_i: difference to the mean of movie ratings 

cent_movie_ratings <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

cent_movie_ratings %>%
  ggplot(aes(b_i)) +
  geom_histogram(bins=50, color="black") +
  xlab("b_i") +
  ylab("Number of movies") +
  ggtitle("Number of movies by b_i")

pred_movie_effect <- mu + validation %>%
  left_join(cent_movie_ratings, by='movieId') %>%
  .$b_i

# Calculating the RMSE with prediction as Y_i = mu + b_i + eps_i

rmse_movie_effect <- data.frame(method="Movie effect model", RMSE=RMSE(pred_movie_effect,validation$rating))

kable(rmse_movie_effect)

kable(bind_rows(rmse_naive,rmse_movie_effect))


#################################################
# Accounting for user effect: some users tend to
# give ratings systematically higher than others,
# thus leading to a right skew
#################################################

# b_j: difference to the mean of user ratings 

cent_user_ratings <- edx %>%
  group_by(userId) %>%
  summarize(b_j = mean(rating - mu))

cent_user_ratings %>%
  ggplot(aes(b_j)) +
  geom_histogram(bins=50, color="black") +
  xlab("b_j") +
  ylab("Number of users") +
  ggtitle("Nb of users by b_j")

pred_user_effect <- mu + validation %>%
  left_join(cent_user_ratings, by='userId') %>%
  .$b_j

# Calculating the RMSE with prediction as Y_j = mu + b_j + eps_j

rmse_user_effect <- data.frame(method="User effect model", RMSE=RMSE(pred_user_effect,validation$rating))

kable(rmse_user_effect)

kable(bind_rows(rmse_naive,rmse_movie_effect, rmse_user_effect))

#########################
# Accounting for combined
# movie and user effects
#########################

cent_mvus_ratings <- edx %>%
  left_join(cent_movie_ratings, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_ij = mean(rating - mu - b_i))

pred_mvus_effect <- validation %>%
  left_join(cent_movie_ratings, by='movieId') %>%
  left_join(cent_user_ratings, by='userId') %>%
  mutate(pred=mu + b_i + b_j) %>%
  .$pred

# Calculating the RMSE with prediction as Y_{i,j} = mu + b_i + b_j + eps_{i,j}

rmse_mvus_effect <-data.frame(method="Combined movie and user effect model", RMSE=RMSE(pred_mvus_effect,validation$rating))

kable(rmse_mvus_effect)

kable(bind_rows(rmse_naive,rmse_movie_effect, rmse_user_effect, rmse_mvus_effect))


############################################################
# Accounting for combined movie, user and movie age effects:
# the age effect traduces the difference of appreciation of
# movies depending on how old they are at time of rating
############################################################

cent_mvusage_ratings <- edx %>%
  left_join(cent_movie_ratings, by='movieId') %>%
  left_join(cent_mvus_ratings, by='userId') %>%
  group_by(movie_age) %>%
  summarize(b_k = mean(rating - mu - b_i - b_ij))

# Making an organized list of (unique) ages to enable left_join between both tables

uniq_ages <- unique(cent_mvusage_ratings$movie_age)

validation$movie_age <- sapply(validation$movie_age, function(x){uniq_ages[which.min(abs(uniq_ages-x))]})

pred_mvusage_effect <- validation %>% 
  left_join(cent_movie_ratings, by='movieId') %>%
  left_join(cent_user_ratings, by='userId') %>%
  left_join(cent_mvusage_ratings, by='movie_age') %>%
  mutate(pred = mu + b_i + b_j + b_k) %>%
  .$pred

# Calculating the RMSE with prediction as Y_{i,j,k} = mu + b_i + b_j + b_k + eps_{i,j,k}

rmse_mvusage_effect <- data.frame(method="Combined movie, user and age effect model", RMSE=RMSE(pred_mvusage_effect,validation$rating))

kable(rmse_mvusage_effect)

kable(bind_rows(rmse_naive,rmse_movie_effect, rmse_user_effect, rmse_mvus_effect, rmse_mvusage_effect))


#######################################################
# Accounting for joint movie, user, movie age and
# rating hour effects : the last effect corresponds
# to the difference to average of the rating depending
# on the time of the day (or night) at rating time
#######################################################

cent_mvusagehour_ratings <- edx %>%
  left_join(cent_movie_ratings, by='movieId') %>%
  left_join(cent_mvus_ratings, by='userId') %>%
  left_join(cent_mvusage_ratings, by='movie_age') %>%
  group_by(rate_hour) %>%
  summarize(b_l = mean(rating - mu - b_i - b_ij - b_k))

# Making an organized list of (unique) rating hours to enable left_join between both tables

uniq_hours <- unique(cent_mvusagehour_ratings$rate_hour)

validation$rate_hour <- sapply(validation$rate_hour, function(x){uniq_hours[which.min(abs(uniq_hours-x))]})

pred_mvusagehour_effect <- validation %>% 
  left_join(cent_movie_ratings, by='movieId') %>%
  left_join(cent_user_ratings, by='userId') %>%
  left_join(cent_mvusage_ratings, by='movie_age') %>%
  left_join(cent_mvusagehour_ratings, by='rate_hour') %>%
  mutate(pred = mu + b_i + b_j + b_k + b_l) %>%
  .$pred

# Calculating the RMSE with prediction as Y_{i,j,k,l} = mu + b_i + b_j + b_k + b_l + eps_{i,j,k,l}

rmse_mvusagehour_effect <- data.frame(method="Combined movie, user, movie age and rating hour effect model", RMSE=RMSE(pred_mvusagehour_effect,validation$rating))

kable(rmse_mvusagehour_effect)

kable(bind_rows(rmse_naive,rmse_movie_effect, rmse_user_effect, rmse_mvus_effect, rmse_mvusage_effect, rmse_mvusagehour_effect))

#######################################################################
# Regularizing the various effects, by accounting for the number of
# occurences for each effect and applying a penalty term for low ones:
# obscure movies rated only a few times should be considered as more
# difficult to predict regarding their future ratings; 
# this function can take a couple of minutes to  run through 
#######################################################################

lambdas <- seq(0, 10, 0.25)

reg_rmses <- sapply(lambdas, function(r){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + r))
  
  b_j <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_j = sum(rating - b_i - mu) / (n() + r))
  
  b_k <- edx %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_j, by="userId") %>%
    group_by(movie_age) %>%
    summarize(b_k = sum(rating - b_i - b_j - mu) / (n() + r))
  
  b_l <- edx %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_j, by="userId") %>%
    left_join(b_k, by="movie_age") %>%
    group_by(rate_hour) %>%
    summarize(b_l = sum(rating - b_i - b_j - b_k - mu) / (n() + r))
  
  pred_mvusagehour_reg <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_j, by = "userId") %>%
    left_join(b_k, by = "movie_age") %>%
    left_join(b_l, by = "rate_hour") %>%
    mutate(pred = mu + b_i + b_j + b_k + b_l) %>%
    .$pred
  
  return(RMSE(pred_mvusagehour_reg, validation$rating))
})

# Plotting the different values of the regularized RMSE for each penalty factor level

qplot(lambdas, reg_rmses)

# Identifying the RMSE-minimizing lambda

lambdas[which.min(reg_rmses)]

rmse_mvusagehour_reg <- data.frame(method="Regularized (lambda = 5.5) combined movie, user, movie age and rating hour effect model", RMSE = min(reg_rmses))

# Displaying the final RMSE level, on the four-facot model with regularization

kable(rmse_mvusagehour_reg)

kable(bind_rows(rmse_naive,rmse_movie_effect, rmse_user_effect, rmse_mvus_effect, rmse_mvusage_effect, rmse_mvusagehour_effect, rmse_mvusagehour_reg))

# Saving the various objects created for the Rmarkdown report 

save.image()

# END