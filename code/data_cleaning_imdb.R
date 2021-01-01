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

# Histogram of movie released by year
ggplot(movie_raw, aes(title_year)) +
  geom_bar() +
  labs(x = "Year movie was released", y = "Movie Count", title = "Histogram of Movie released") 
#We notice that most of the movies in the data are released after 1960. Thefore, we decide to remove movies with a release date before 1960.

# Delete films before 1960
movie_raw <- movie_raw[movie_raw$title_year >= 1960,]

# Check for summary 
summary(movie_raw )
# We keep 3834 observations of 27 variables.

# Save the raw data file
my_path <- "~/Documents/GitHub/Final_Project_Pauline_Broussolle/data/"
write_csv( movie_raw, paste0(my_path,"clean/movie_data.csv"))

