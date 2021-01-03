# IMDB scores analysis

This folder contains codes for the prediction of movie scores on IMDB.

data_cleaning_imdb.R - loads the raw movie data, which is the IMDB 5000 Movies dataset from Kaggle. Cleans data and saves it into a new file on data/clean folder.

data_cleaning_sample.R - loads the raw movie data, cleans it and creates a data sample of movies from countries other than USA and released between 1922 and 2000. Saves data into a new file on data/clean folder.

imdb_analysis.Rmd - loads the clean data, check the summary statistics and distribution of the variable imdb_score, and then executes three different regression models on the chosen variables. It presents the chosen model, and then carry a robustness check and analyse of the residuals.
