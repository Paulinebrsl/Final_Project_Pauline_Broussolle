#### Assignment 2 DA2 ###

## Data Cleaning

library(tidyverse)

#Import raw data
my_path <- "~/Documents/GitHub/Final_Project_Pauline_Broussolle/data/"
movie_raw <- read_csv(paste0(my_path,'raw/movie_metadata.csv'))
View(movie_raw)
glimpse(movie_raw)

# Remove variables we will not use
movie_raw <- select(movie_raw, -c(movie_imdb_link, aspect_ratio))

# Check and remove null variables and duplicates
colSums(sapply(movie_raw, is.na))
# We choose to remove null variables from 2 important variables: gross and budget
movie_raw <- movie_raw[!is.na(movie_raw$gross), ]
movie_raw <- movie_raw[!is.na(movie_raw$budget), ]
colSums(sapply(movie_raw, is.na))

sum(duplicated(movie_raw))
movie_raw <- movie_raw[!duplicated(movie_raw), ]
# Now we have 3857 observations.


#Add variables : profit and return on investment (percentage)
movie_raw  <-movie_raw  %>% 
  mutate(profit = gross - budget,
         return_on_investment = (profit/budget)*100)

# Is the color of a movie an important factor? 
table(movie_raw$color)
#Over 95% movies are in color, which means this variable is nearly constant: we choose to remove this variable from our dataset.
movie_raw <- subset(movie_raw, select = -c(color))

# Is language an essential variable ? 
table(movie_raw$language)
#Over 95% movies are in English, which means this variable is nearly constant: we choose to remove this variable from our dataset.
movie_raw <- subset(movie_raw, select = -c(language))

# Is the country of origin an essential variable? 
table(movie_raw$country)
#We notice that movies are mainly from the USA (almost 80%). We have decided to group the data into 2 groups: USA and others to have less levels.
levels(movie_raw$country) <- c(levels(movie_raw$country), "Others")
movie_raw$country[(movie_raw$country != 'USA')] <- 'Others' 
movie_raw$country <- factor(movie_raw$country)
table(movie_raw$country)


movie_raw <- filter( movie_raw , 
                        country == "Others" )

movie_raw <- filter( movie_raw , 
                     title_year < "2010" )

# 1 if duration is superior to 120 min and 0 otherwise
movie_raw  <- movie_raw %>% mutate( duration= 1*(duration>120))
# 1 if title year is superior to 2000 and 0 otherwise
movie_raw  <- movie_raw  %>% mutate( title_year= 1*(title_year>2000))

# Create dummy variables for genre
movie_raw $Action <- ifelse(movie_raw$genre_1 == 'Action', 1, 0) 
movie_raw $Adventure <- ifelse(movie_raw$genre_1 == 'Adventure', 1, 0) 
movie_raw $Biography <- ifelse(movie_raw$genre_1 == 'Biography', 1, 0) 
movie_raw $Comedy <- ifelse(movie_raw$genre_1 == 'Comedy', 1, 0) 
movie_raw$Drama <- ifelse(movie_raw$genre_1 == 'Drama', 1, 0) 
movie_raw$Horror <- ifelse(movie_raw$genre_1 == 'Horror', 1, 0) 
movie_raw$Documentary <- ifelse(movie_raw$genre_1 == 'Documentary', 1, 0)
movie_raw  <- subset(movie_raw , select = -c(genre_2))

# Remove plot keywords and actors name 
movie_raw  <- subset(movie_raw , select = -c(actor_2_name, actor_1_name, actor_3_name, plot_keywords))

#We reorganize to have variables less correlated between them
movie_raw$other_actors_facebook_likes <- movie_raw$actor_2_facebook_likes + movie_raw$actor_3_facebook_likes
movie_raw$critic_review_ratio <- movie_raw$num_critic_for_reviews / movie_raw$num_user_for_reviews
movie_raw  <- subset(movie_raw, select = -c(actor_2_facebook_likes, actor_3_facebook_likes, facenumber_in_poster,num_user_for_reviews, actor_1_facebook_likes ))

# Take Logs 
movie_raw <- movie_raw %>% mutate( ln_profit = log( profit ),
                     ln_gross = log( gross ),
                     ln_num_voted_users = log( num_voted_users ),
                     ln_num_critic_for_reviews = log(num_critic_for_reviews))
movie_raw <- subset(movie_raw , ln_profit!= -Inf)

# We keep 602 observations of 27 variables.

# Save the raw data file
my_path <- "~/Documents/GitHub/Final_Project_Pauline_Broussolle/data/"
write_csv( movie_raw, paste0(my_path,"clean/movie_sample.csv"))

