
# Clear memory
rm(list=ls())

# Load packages
library(tidyverse)
require(scales)
library(lspline)
library(estimatr)
library(texreg)
library(ggthemes)
library(WDI)
library(magrittr)
library(moments)
library(gridExtra)

my_path <- "~/Documents/GitHub/Final_Project_Pauline_Broussolle/data/"
dm <- read_csv(paste0(my_path,'clean/movie_data.csv'))

# 2) What are the possible problems with the data - data quality
#   - Representative sample?
#       - Half of the Massachusetts schools - representative only for this state
#   - Measurement errors in the variables?
#       - In general, we do not have variable on the size of the school:
#           - matter for salary, income/capita and for score(s)
#   - What they truly capture and what you want to use them for?
#       - Expenditures also capture the size of the school...

#####
# Model setup
# Outcome variable:      imdb_score  
# Parameter of interest: profit, gross, critic_for-review, duration, genre
#
# Thinking about potential confounders:
#   - Number of people who voted, language 
#   - budget, return on investment
#
# 3) Descriptives
#
# Quick check on all HISTOGRAMS
dm %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

summary( dm )

# Check the main parameter of interests and potential confounders:


# Create a dummy variable from duration and genre:
table(dm$genre_1)
table(dm$duration)
dm <- subset(dm, select = -c(genre_2))

# 1 if duration is inferior to 120 min
# 0 otherwise
dm <- dm %>% mutate( duration= 1*(duration<120))

table(dm$title_year)
dm <- dm %>% mutate( title_year= 1*(title_year>2000))

# 1) Nb of death - confirmed cases: level-level model without scaling
ggplot( dm , aes(x = num_critic_for_reviews, y = imdb_score)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "movie gross",y = "IMDB score", title="Level-level model")+
  theme(text = element_text(size = 9))

ggplot( dm , aes(x = num_critic_for_reviews, y = imdb_score)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "movie gross, ln scale",y = "IMDB score", title="Level-log model")+
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  theme(text = element_text(size = 9))

ggplot( dm , aes(x = profit, y = imdb_score)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "movie gross, ln scale",y = "IMDB score", title="Level-log model")+
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  theme(text = element_text(size = 9))

ggplot( dm , aes(x = gross, y = imdb_score)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "movie gross, ln scale",y = "IMDB score", title="Level-log model")+
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  theme(text = element_text(size = 9))

table(dm$gross)

ggplot( dm , aes(x = num_voted_users, y = imdb_score)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "movie gross, ln scale",y = "IMDB score", title="Level-log model")+
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  theme(text = element_text(size = 9))

dm <- dm %>% mutate( ln_profit = log( profit ),
                     ln_gross = log( gross ),
                     ln_num_voted_users = log( num_voted_users ),
                     ln_num_critic_for_reviews = log(num_critic_for_reviews))
dm <- subset(dm, ln_profit!= -Inf)

reg1 <- lm (imdb_score ~ num_critic_for_reviews+ num_voted_users + profit + budget + duration + gross + return_on_investment + country, data = dm)
summary( reg1 )
ggplot( data = dm, aes( x = imdb_score, y = ln_gross) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

reg2 <- lm (imdb_score ~ num_critic_for_reviews+ num_voted_users + profit + gross +country, data = dm)
summary( reg2 )

reg3 <- lm (imdb_score ~ duration + genre_1 + gross+ num_voted_users, data = dm)
summary( reg3 )

# 4) Comparing explanatory variables 
#
# Check the correlations
#
numeric_dm <- keep( dm , is.numeric )
cT <- cor(numeric_dm , use = "complete.obs")

# Check for highly correlated values:
sum( cT >= 0.8 & cT != 1 ) / 2
# Find the correlations which are higher than 0.8
id_cr <- which( cT >= 0.8 & cT != 1 )
pair_names <- expand.grid( variable.names(numeric_dm) , variable.names(numeric_dm) )
# Get the pairs:
high_corr <- pair_names[ id_cr , ]
high_corr <- mutate( high_corr , corr_val = cT[ id_cr ] )
high_corr



reg5 <- lm (imdb_score ~ ln_num_voted_users , data = dm)
summary( reg5)

reg4 <- lm_robust (imdb_score ~ gross , data = dm)
summary( reg4)

         
reg6 <- lm (imdb_score ~ ln_gross + ln_profit + ln_num_voted_users + ln_num_critic_for_reviews + title_year, data = dm)
summary( reg7)

reg7 <- lm (imdb_score ~ ln_gross+ln_num_voted_users + ln_profit + ln_num_critic_for_reviews + title_year + Action + Adventure + Drama + Comedy + Horror, data = dm)
summary( reg7, vcov=sandwich)
par(mfrow=c(2,2))
plot(reg7)

#1) y_hat-y plot - use reg5
# You may try: - not handling missing values properly...
dm <- mutate( dm , y_hat = reg7$fitted.values )

# Predict is more general and can handle missing values...
dm <- mutate( dm , y_hat = predict( reg7 , dm ) )

#y_hat-y plot
ggplot( data = dm ) +
  geom_point (aes( x = y_hat , y = imdb_score ) ,  color="red")+
  geom_line( aes( x = imdb_score , y =imdb_score ) , color = "navyblue" , size = 1.5 )+
  labs( x = "Predicted test scores", y = "Actual test scores")

# Get BIC and AIC measures for the model:
#
# Unfortunately lm_robust does not have this... 
# You can use simple lm (remember, in this case SEs are not important!)

# Does adding wealth measure increase the prediction?
reg4_lm <- lm( score4 ~ lspline( stratio , 18 ) + english_d 
               + lspline(lunch,15) + special , data = subset(df,complete.cases(df) ) )

reg5_lm <- lm( score4 ~ lspline( stratio , 18 ) + english_d 
               + lspline(lunch,15) + special 
               + lspline(salary,c(35,40)) + exptot 
               + log( income ) + scratio , data = subset(df,complete.cases(df) ) )

BIC(reg7)
AIC(reg7)

table(dm$genre_1)

dm$Action <- ifelse(dm$genre_1 == '', 1, 0) 
dm$Adventure <- ifelse(dm$genre_1 == 'Adventure', 1, 0) 
dm$Biography <- ifelse(dm$genre_1 == 'Biography', 1, 0) 
dm$Comedy <- ifelse(dm$genre_1 == 'Comedy', 1, 0) 
dm$Drama <- ifelse(dm$genre_1 == 'Drama', 1, 0) 
dm$Horror <- ifelse(dm$genre_1 == 'Horror', 1, 0) 
dm$Documentary <- ifelse(dm$genre_1 == 'Documentary', 1, 0)

table(dm$content_rating)

#Data pre-processing
#REMOVING IRRELEVANT VARIABLES

# Can we do some analysis based on producers and actors name ?
sum(uniqueN(dm$director_name))
sum(uniqueN(dm[, c("actor_1_name","actor_2_name","actor_3_name")]))
dm <- subset(dm, select = -c(actor_2_name, actor_1_name, actor_3_name, plot_keywords))
#We decided to remove actors, producers and IMdB movie link as they are irrelevant data here


#We have to find the correlation between the variables in order to spot and remove the high correlated variables 
ggcorr(dm, label = TRUE, label_round = 2, label_size = 1, size = 2, hjust = .85) +
  ggtitle("Correlation Heatmap")+
  theme(plot.title = element_text(hjust = 0.5))

#We reorganize to have variables less correlated between them
dm$other_actors_facebook_likes <- dm$actor_2_facebook_likes + dm$actor_3_facebook_likes
dm$critic_review_ratio <- dm$num_critic_for_reviews / dm$num_user_for_reviews
dm <- subset(dm, select = -c(actor_2_facebook_likes, actor_3_facebook_likes))
View(dm)
#We have the new correlation heatmap
library(corrplot)
library(GGally)
ggcorr(dm, label = TRUE, label_round = 2, label_size = 4, size = 3, hjust = .85) +
  ggtitle("Correlation Heatmap")+
  theme(plot.title = element_text(hjust = 0.5))

dataAV <- data.frame(Action=1, Adventure=0, Drama=0, Biography=0, Horror=0, Documentary=0, duration=0, gross=760505847, num_voted_users=886204, profit=523505847)
predict(reg4, dataAV,interval="predict")

dataGR <- data.frame(Action=0, Adventure=1, Drama=0, Biography=0, Horror=0, Documentary=0, duration=1, gross=274084951, num_voted_users=582917, profit=	174084951)
predict(reg4, dataGR,interval="predict")

dataG <- data.frame(Action=1, Adventure=0, Drama=0, Biography=0, Horror=0, Documentary=0, duration=0, gross=116593191, num_voted_users=223393, profit=-83406809)
predict(reg4, dataG,interval="predict")

datadevil <- data.frame(Action=0, Adventure=0, Drama=0, Biography=0, Horror=1, Documentary=0, duration=1, gross=53245055, num_voted_users=30570, profit=52245055)
predict(reg4, datadevil,interval="predict")

datadevil2 <- data.frame(ln_gross=17.79042, ln_num_voted_users=10.327774 ,ln_profit=17.77146,ln_num_critic_for_reviews=5.416100, title_year=1)
predict(reg6, datadevil2,interval="predict")

datadevil2 <- data.frame(ln_gross=16.57672, ln_num_voted_users=9.540579 ,ln_profit=15.99242,ln_num_critic_for_reviews=4.919981, title_year=1)
predict(reg6, datadevil2,interval="predict")

datap <- data.frame(ln_gross=19.86296, ln_num_voted_users=13.16550 ,ln_profit=19.10394,ln_num_critic_for_reviews=5.746203, title_year=1)
predict(reg6, datap,interval="predict")

# Check predicted probabilities: is there any interesting values?
# predicted probabilities
dm$pred_reg3 <- predict(reg3)
# Summary
summary(dm$pred_reg3)

# Show the predicted probabilities' distribution
ggplot(data=dm, aes(x=pred_reg3)) +
  geom_histogram( aes( y = ..density.. ),  fill= "darkorange",col="black", alpha=0.7, binwidth =0.5) +
  labs(x = "Predicted scores",y = "Absolute frequency")

# We are interested in the top 1% and bottom 1% characteristics!
#   Is there any significant difference?

# Create bins which categorize the predicted values between 1-100
share <- share %>% 
  mutate(q100_pred_lpm = ntile(pred_lpm, 100))

# Make a summary statistics, using sum_stat for the bottom (q100_pred_lpm==1) 
#   and top 1% (q100_pred_lpm==100), using stats = c('mean','median','sd')
#   and variables c('smoking','ever_smoked','female','age','eduyears','income10','bmi','exerc')
#   use the num_obs = F input for sum_stat

# Bottom 1% means low probability of stayhealthy
# 'smoking','ever_smoked','female','age','eduyears','income10','bmi','exerc'
b1 <- share %>% filter( q100_pred_lpm == 1 )
var_interest <- c('smoking','ever_smoked','female','age','eduyears','income10','bmi','exerc')
stat_interest <- c('mean','median','sd')
sum_stat(b1,var_interest,stat_interest,num_obs = F)

# Top 1% means high probability of stayhealthy
t1 <- share %>% filter( q100_pred_lpm == 100 )
sum_stat(t1,var_interest,stat_interest,num_obs = F)

# You may change the variable names to remove...
rm(lpm3,t1,b1,stat_interest,var_interest)


dataAV <- data.frame(Action=1, Adventure=0, Drama=0, Comedy=0, Horror=0, duration=0, ln_gross=20.44949, ln_num_voted_users=13.69470, ln_profit=20.07606, ln_num_critic_for_reviews=6.583409)

predict(reg3, dataAV, interval ="confidence" , alpha = 0.07)

dataT <- data.frame(Action=0, Adventure=0, Drama=1, Comedy=0, Horror=0, duration=0, ln_gross=20.30574, ln_num_voted_users=13.58365, ln_profit=19.94385, ln_num_critic_for_reviews=5.752573)

predict(reg3, dataT, interval ="confidence" , alpha = 0.05 )


set.seed(123)
index1 <- sample(1:nrow(dm), 403)
index1





