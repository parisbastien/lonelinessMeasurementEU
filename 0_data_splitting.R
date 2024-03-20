library(caret)
library(readr)

#-----------------------
#replace NA with the random number of your choice
random_seed <- NA

#replace NA with the filename of the dataset saved in .csv format (e.g., "EU_loneliness_survey.csv")
filename <- NA

#Once done, run the R script in a folder that contains the .csv file of the data. This should create the "exploratory_data.csv" and "confirmatory_data.csv" files.
#-----------------------

#setting a seed for reproducibility
set.seed(random_seed)

#importing the data
data <- read_csv(filename) 

#splitting the data into two separate datasets of equal sample sizes
#stratification is performed on the "country" variable to ensure similarities between datasets in terms of country
index <- createDataPartition(data$country, p = 0.5, list = FALSE, times = 1) 
exploratory_data <- data[index, ]
confirmatory_data <- data[-index, ]

#exporting the data
write_csv(exploratory_data, "exploratory_data.csv")
write_csv(confirmatory_data, "confirmatory_data.csv")