rm(list = ls())
library(readr)
library(lubridate)
library(dplyr)
library(psych)
library(EFA.dimensions)
library(lavaan)
library(openxlsx)
library(semPlot)
library(semTools)
library(mixmgfa)
library(magrittr)
source("MixtureMG_FA.R")
source("MixtureMG_FA_intercepts.R")
source("MixtureMG_FA_loadings.R")
source("MixtureMG_FA_loadingsandintercepts.R")
set.seed(1)


#----------------------------------------------------
#data import
#----------------------------------------------------
#change the value to "confirmatory_data.csv" to run the analyses on the confirmatory dataset
filename <- "exploratory_data.csv"
data <- read_csv(filename)
#----------------------------------------------------

#----------------------------------------------------
#variables recoding
#----------------------------------------------------
#recoding of the country variable
countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
  "Czechia", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Ireland", "Italy",
  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
  "Spain", "Sweden"
)
data$country_chr <- data$country

#variable names
variables <- c("loneliness_djg_a", "loneliness_djg_b", "loneliness_djg_c", "loneliness_djg_d", "loneliness_djg_e", "loneliness_djg_f", "loneliness_ucla_a", "loneliness_ucla_b", "loneliness_ucla_c", "social_support_a", "social_support_b", "social_support_c", "social_support_d", "health_general", "feelings_depr", "feelings_happy", "family_meet_face", "family_meet_tele", "friends_meet_face", "friends_meet_tele", "family_n__1__open", "friends_n__1__open", "neighbours", "social_activities_a")

# Loop over each variable to create a frequency table
for (variable in variables) {
  cat("Frequency table for", variable, ":\n")
  print(table(data[[variable]]))
  cat("\n") # Print a newline for better readability between tables
}

#Turning 997 (not applicable), 998 (don't know), and 999 (prefer not to say) into NAs
for (variable in c("loneliness_djg_a", "loneliness_djg_b", "loneliness_djg_c", "loneliness_djg_d", "loneliness_djg_e", "loneliness_djg_f", "loneliness_ucla_a", "loneliness_ucla_b", "loneliness_ucla_c", "social_support_a", "social_support_b", "social_support_c", "social_support_d", "health_general", "feelings_depr", "feelings_happy", "family_meet_face", "family_meet_tele", "friends_meet_face", "friends_meet_tele", "family_n__1__open", "friends_n__1__open", "neighbours", "social_activities_a", "loneliness_direct")){
  data[[variable]] <- ifelse(data[[variable]] == "Prefer not to say", NA, data[[variable]])
  data[[variable]] <- ifelse(data[[variable]] == "Don’t know", NA, data[[variable]])
  data[[variable]] <- ifelse(data[[variable]] == "Not applicable", NA, data[[variable]])
}

# Custom recoding function with explicit mappings for each variable group
recode_variable <- function(x, name) {
  # Define the mappings for each group of variables
  mappings <- list(
    "loneliness_djg" = c("No" = 0, "More or less" = 1, "Yes" = 2),
    "loneliness_ucla" = c("Hardly ever or never" = 0, "Some of the time" = 1, "Often" = 2),
    "social_support" = c("None of the time" = 1, "A little of the time" = 2, "Some of the time" = 3, "Most of the time" = 4, "All of the time" = 5),
    "health_general" = c("Very poor" = 1, "Fairly poor" = 2, "Average" = 3, "Fairly good" = 4, "Very good" = 5),
    "meet" = c("Never" = 0, "Every two months or less frequently" = 1, "Once a month" = 2, "Every two weeks" = 3, "Every week" = 4, "More than once a week" = 5, "Daily" = 6),
    "social_activities_a" = c("Never" = 0, "Every two months or less frequently" = 1, "Once a month" = 2, "Every two weeks" = 3, "Every week" = 4, "More than once a week" = 5, "Daily" = 6),
    "neighbours" = c("Never or hardly ever" = 0, "Less than once a month" = 1, "1-3 times a month" = 2, "About once a week" = 3, "Several times a week" = 4, "Almost every day" = 5),
    "feelings" = c("Never" = 0, "Very rarely" = 1, "Rarely" = 2, "Occasionally" = 3, "Very frequently" = 4, "Always" = 5),
    "loneliness_direct" = c("None of the time" = 0, "A little of the time" = 1, "Some of the time" = 2, "Most of the time" = 3, "All of the time" = 4)
  )
  
  # Determine the correct mapping based on the variable name
  for (prefix in names(mappings)) {
    if (grepl(prefix, name)) {
      map <- mappings[[prefix]]
      if(is.character(x)) x <- factor(x, levels = names(map))
      return(as.numeric(map[as.character(x)]))
    }
  }
  
  # Return original if no mapping found
  return(x)
}

# Apply the recoding function to each column in 'data'
names(data) <- make.names(names(data)) # Ensure valid column names
for (name in names(data)) {
  data[[name]] <- recode_variable(data[[name]], name)
}

#recoding of variables so that higher scores indicate greater loneliness (DJGLS-6)
for(variable in c("loneliness_djg_d", "loneliness_djg_e", "loneliness_djg_f")){
  data[[variable]] <- ifelse(is.na(data[[variable]]), NA, 2 - data[[variable]])
}


# Missing data ------------------------------------------------------------
loneliness_data <- data[grep("ucla|djg|direct", names(data), ignore.case = TRUE)]
# Calculate and print the percentage of NA values in the subset
print((sum(is.na(loneliness_data)) / (nrow(loneliness_data) * ncol(loneliness_data))) * 100)

# Table 1 -----------------------------------------------------------------

# Calculate age
data$age <- year(Sys.Date()) - data$date_birth_year - (month(Sys.Date()) < data$date_birth_month)

# Calculate loneliness scores
data$loneliness_djgls_6 <- rowSums(data[,c("loneliness_djg_a", "loneliness_djg_b", "loneliness_djg_c", "loneliness_djg_d", "loneliness_djg_e", "loneliness_djg_f")], na.rm = TRUE)
data$loneliness_t_ils <- rowSums(data[,c("loneliness_ucla_a", "loneliness_ucla_b", "loneliness_ucla_c")], na.rm = TRUE)

# Calculate statistics by country
statistics_by_country <- data %>%
  group_by(country) %>%
  summarise(
    n = n(),
    age_median = median(age, na.rm = TRUE),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    loneliness_djgls_6_median = median(loneliness_djgls_6, na.rm = TRUE),
    loneliness_djgls_6_mean = mean(loneliness_djgls_6, na.rm = TRUE),
    loneliness_djgls_6_sd = sd(loneliness_djgls_6, na.rm = TRUE),
    loneliness_t_ils_median = median(loneliness_t_ils, na.rm = TRUE),
    loneliness_t_ils_mean = mean(loneliness_t_ils, na.rm = TRUE),
    loneliness_t_ils_sd = sd(loneliness_t_ils, na.rm = TRUE),
    loneliness_direct_median = median(loneliness_direct, na.rm = TRUE),
    loneliness_direct_mean = mean(loneliness_direct, na.rm = TRUE),
    loneliness_direct_sd = sd(loneliness_direct, na.rm = TRUE)
  ) 

statistics_overall <- data %>%
  summarise(
    country = "Overall",
    n = n(),
    age_median = median(age, na.rm = TRUE),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    loneliness_djgls_6_median = median(loneliness_djgls_6, na.rm = TRUE),
    loneliness_djgls_6_mean = mean(loneliness_djgls_6, na.rm = TRUE),
    loneliness_djgls_6_sd = sd(loneliness_djgls_6, na.rm = TRUE),
    loneliness_t_ils_median = median(loneliness_t_ils, na.rm = TRUE),
    loneliness_t_ils_mean = mean(loneliness_t_ils, na.rm = TRUE),
    loneliness_t_ils_sd = sd(loneliness_t_ils, na.rm = TRUE),
    loneliness_direct_median = median(loneliness_direct, na.rm = TRUE),
    loneliness_direct_mean = mean(loneliness_direct, na.rm = TRUE),
    loneliness_direct_sd = sd(loneliness_direct, na.rm = TRUE)
  )

statistics_by_country <- rbind(statistics_by_country, statistics_overall)

#save the results to a CSV file
write.csv(statistics_by_country, "output_statistics_by_country.csv")

#----------------------------------------------------
#FACTOR ANALYSES AND INTERNAL CONSISTENCY
#----------------------------------------------------

#----------------
#DJGLS-6
#----------------
#keeping only the items of interest, and removing rows with missing values
data_djg <- subset(data, select = c(loneliness_djg_a, loneliness_djg_b, loneliness_djg_c, loneliness_djg_d, loneliness_djg_e, loneliness_djg_f, w_country_04))
data_djg <- na.omit(data_djg)

djg_model_2F <- 
'emotionalLoneliness =~ loneliness_djg_a + loneliness_djg_b + loneliness_djg_c
socialLoneliness =~ loneliness_djg_d + loneliness_djg_e + loneliness_djg_f
emotionalLoneliness ~~ socialLoneliness'

#confirmatory factor analysis across all EU member states, on the a-priori factor structure
fit_djg_model <- cfa(model = djg_model_2F, 
                     data = data_djg,
                     ordered = TRUE,
                     estimator = "WLSMV",
                     sampling.weights = "w_country_04")
summary(fit_djg_model, standardized = TRUE)
fitmeasures(fit_djg_model, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

#parallel analysis
djg_paralell <- fa.parallel(x = data_djg,
                            cor = "poly") #3-point Likert-type measures are best handled with polychoric correlations

#Empirical Kaiser Criterion
djg_empkc <- EMPKC(data = data_djg,
                   corkind = "polychoric")


#we will edit the following model after identifying the optimal factor structure, balancing theoretical parsimony with model fit
djg_model <- 'loneliness =~ loneliness_djg_a + loneliness_djg_b + loneliness_djg_c + loneliness_djg_d + loneliness_djg_e + loneliness_djg_f'
#we will edit the following value after identifying the optimal factor structure, balancing theoretical parsimony with model fit
djg_factors <- 2

#confirmatory factor analysis across all EU member states
fit_djg_modelUni <- cfa(model = djg_model, 
                     data = data_djg,
                     ordered = TRUE,
                     estimator = "WLSMV",
                     sampling.weights = "w_country_04")
summary(fit_djg_modelUni, standardized = TRUE)
fitmeasures(fit_djg_modelUni, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

#internal consistency across all EU member states
ic_djg <- semTools::compRelSEM(fit_djg_model)
ic_djgUni <- semTools::compRelSEM(fit_djg_modelUni)
lavTestLRT(fit_djg_model, fit_djg_modelUni)

#confirmatory factor analysis and internal consistency for each member state separately
djg_per_state <- list()
for(country in countries){
  #keeping participants who belong to the country of interest, the items of interest, and removing rows with missing values
  temp_data <- data[data$country_chr == country, ]
  temp_data <- subset(temp_data, select = c(loneliness_djg_a, loneliness_djg_b, loneliness_djg_c, loneliness_djg_d, loneliness_djg_e, loneliness_djg_f))
  temp_data <- na.omit(temp_data)
  
  #confirmatory factor analysis
  temp_fit <- cfa(model = djg_model_2F, 
                  data = temp_data,
                  ordered = TRUE,
                  estimator = "WLSMV")
  
  temp_paralell <- fa.parallel(x = data_djg, plot = FALSE,
                              cor = "poly")$nfact #3-point Likert-type measures are best handled with polychoric correlations
  
  #Empirical Kaiser Criterion
  temp_empkc <- EMPKC(data = data_djg, verbose = FALSE,
                     corkind = "polychoric")$NfactorsEMPKC
  
  
  #internal consistency
  temp_ic <- semTools::compRelSEM(temp_fit)
  
  #adding the results in a list
  temp_list <- list(cfa=fitmeasures(temp_fit, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper")), paralell = temp_paralell, KC = temp_empkc, ic=temp_ic)
  djg_per_state[[country]] <- temp_list
}

#internal consistency descriptives
describe(unlist(lapply(djg_per_state, function(x)x$ic[1])))
describe(unlist(lapply(djg_per_state, function(x)x$ic[2])))

djg_per_state

#----------------
#T-ILS
#----------------
#keeping only the items of interest, and removing rows with missing values
data_tils <-subset(data, select = c(loneliness_ucla_a, loneliness_ucla_b, loneliness_ucla_c, w_country_04))
data_tils <- na.omit(data_tils)

#As the T-ILS is a three-item scale, only a one factor model can fit the data
#As a consequence, we don't run parallel analysis and empirical kaiser criterion extraction techniques on the T-ILS, and directly run the confirmatory factor analysis on the a-priori factor structure
tils_model <- 'loneliness =~ loneliness_ucla_a + loneliness_ucla_b + loneliness_ucla_c'

#confirmatory factor analysis across all EU member states, on the a-priori factor structure
fit_tils_model <- cfa(model = tils_model,
                      data = data_tils,
                      ordered = TRUE,
                      estimator = "WLSMV",
                      sampling.weights = "w_country_04")
summary(fit_tils_model, standardized = T)

#internal consistency across all EU member states
ic_tils <- semTools::compRelSEM(fit_tils_model)

#confirmatory factor analysis and internal consistency for each member state separately
tils_per_state <- list()
for (country in countries){
  #keeping participants who belong to the country of interest, the items of interest, and removing rows with missing values
  temp_data <- data[data$country_chr == country, ]
  temp_data <- subset(temp_data, select = c(loneliness_ucla_a, loneliness_ucla_b, loneliness_ucla_c))
  temp_data <- na.omit(temp_data)
  
  #confirmatory factor analysis
  temp_fit <- cfa(model = tils_model, 
                  data = temp_data,
                  ordered = TRUE,
                  estimator = "WLSMV")
  
  #internal consistency
  temp_ic <- semTools::compRelSEM(temp_fit)
  
  #adding the results in a list
  temp_list <- list(cfa=parameterestimates(temp_fit, standardized = T)[1:3, c(3, 11)], ic=temp_ic)
  tils_per_state[[country]] <- temp_list
}

tils_per_state
describe(unlist(lapply(tils_per_state, function(x)x$ic)))

# Table 2 -----------------------------------------------------------------

# Initialize vectors to store extracted data for djg_per_state
countries <- names(djg_per_state)
chisq_djg <- numeric(length(countries))
cfi_djg <- numeric(length(countries))
rmsea_djg <- numeric(length(countries))
omega_emotional_djg <- numeric(length(countries))
omega_social_djg <- numeric(length(countries))

# Initialize vectors to store extracted data for tils_per_state
factor_loading_a <- numeric(length(countries))
factor_loading_b <- numeric(length(countries))
factor_loading_c <- numeric(length(countries))
omega_tils <- numeric(length(countries))

# Extract data for djg_per_state
for (i in seq_along(countries)) {
  country <- countries[i]
  chisq_djg[i] <- djg_per_state[[country]]$cfa["chisq"]
  cfi_djg[i] <- djg_per_state[[country]]$cfa["cfi"]
  rmsea_djg[i] <- djg_per_state[[country]]$cfa["rmsea"]
  omega_emotional_djg[i] <- djg_per_state[[country]]$ic["emotionalLoneliness"]
  omega_social_djg[i] <- djg_per_state[[country]]$ic["socialLoneliness"]
}

# Extract data for tils_per_state
for (i in seq_along(countries)) {
  country <- countries[i]
  factor_loading_a[i] <- tils_per_state[[country]]$cfa$std.all[tils_per_state[[country]]$cfa$rhs == "loneliness_ucla_a"]
  factor_loading_b[i] <- tils_per_state[[country]]$cfa$std.all[tils_per_state[[country]]$cfa$rhs == "loneliness_ucla_b"]
  factor_loading_c[i] <- tils_per_state[[country]]$cfa$std.all[tils_per_state[[country]]$cfa$rhs == "loneliness_ucla_c"]
  omega_tils[i] <- tils_per_state[[country]]$ic["loneliness"]
}

# Create a dataframe to fill in the Excel table
df <- data.frame(
  Country = countries,
  Chisq_DJG = chisq_djg,
  CFI_DJG = cfi_djg,
  RMSEA_DJG = rmsea_djg,
  Omega_Emotional_DJG = omega_emotional_djg,
  Omega_Social_DJG = omega_social_djg,
  Factor_Loading_A = factor_loading_a,
  Factor_Loading_B = factor_loading_b,
  Factor_Loading_C = factor_loading_c,
  Omega_TILS = omega_tils
)

#save the results to a CSV file
write.csv(df, "model_fit_by_country.csv")


#----------------------------------------------------
#MEASUREMENT INVARIANCE
#----------------------------------------------------

#----------------
#DJGLS-6
#----------------

#testing for measurement invariance (configural; metric; scalar) using multigroup confirmatory factor analysis
#in case measurement invariance fails at any level, we resort to mixture multigroup factor analysis to unravel clusters of countries invariant at the scalar level
data_djg <- subset(data, select = c(country, loneliness_djg_a, loneliness_djg_b, loneliness_djg_c, loneliness_djg_d, loneliness_djg_e, loneliness_djg_f))
data_djg <- na.omit(data_djg)

djg_config <- cfa(model = djg_model_2F,
                  data = data_djg,
                  ordered = TRUE,
                  estimator = "WLSMV",
                  group = "country")
fitmeasures(djg_config, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))


djg_metric <- cfa(model = djg_model_2F,
                  data = data_djg,
                  ordered = TRUE,
                  estimator = "WLSMV",
                  group = "country",
                  group.equal = c("loadings"))
fitmeasures(djg_metric, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
djg_sign_metric <- lavTestLRT(djg_config,djg_metric)


djg_scalar <- cfa(model = djg_model_2F,
                  data = data_djg,
                  ordered = TRUE,
                  estimator = "WLSMV",
                  group = "country",
                  group.equal = c("loadings", "intercepts"))
djg_sign_scalar <- lavTestLRT(djg_metric,djg_scalar)

#recode the coutry names to codes
data_djg[[1]] <- as.numeric(as.factor(data_djg[[1]]))

#The code below is to be run in case measurement invariance failed at any of three levels above
djg_mmg_fa <- mixmgfa(data = as.data.frame(data_djg),
        cluster.spec = c("loadings", "intercepts"),
        nsclust = c(1,6), #invariant at the scalar level
        nfactors = djg_factors, #number of factors to be determined following the factor analyses // mixmgfa is suppposed to identify the appropriate factor structure automatically (to be confirmed by asking Kim De Roover)
        maxiter = 5000, #default values
        nruns = 50, #default value; (important setting to avoid local maxima in case of few groups and/or small groups)
        design = 0, #default value (use of EFAs)
        #rotation = "oblimin", #parameter to add in case nfactors = 2
        preselect = 10)

clusters <- as.data.frame(round(djg_mmg_fa$MMGFAsolutions[[3]]$clustermemberships, 3))
rownames(clusters) <- countries

# Choose the best number of clusters ('K_best') based on the BIC_G and CHull scree ratios and the plots. For plots, use 'plot(OutputObject$overview)'.
# Based on the BIC_G, look for the number of clusters that minimizes the BIC_G or that corresponds to an elbow point in the BIC_G plot (after which the decrease with additional clusters levels off).
# Based on the CHull (scree ratios AND plot), look for the number of clusters that has the maximal scree ratio AND check whether this corresponds to at least a mild elbow point in the lower plot.
# Access the corresponding cluster memberships and parameter estimates by using OutputObject$MMGFAsolutions[[K_best]]$clustermemberships and, for example, OutputObject$MMGFAsolutions[[K_best]]$clusterspecific.loadings.
# The parameter sets are further subdivided in group- and/or cluster-specific parameter sets.

#here we will define the clusters of countries invariant at the scalar level, and subsequently test measurement invariance (configural, metric, scalar) using multigroup confirmatory factor analysis on them
djg_cluster_a_names <- rownames(clusters)[which(clusters$Cluster_1 == 1)] #vector of numerical IDs representing each country that are part of the same cluster (e.g., c(1, 4, 5, 9, 10))
djg_cluster_b_names <- rownames(clusters)[which(clusters$Cluster_2 == 1)] #vector of numerical IDs representing each country that are part of the same cluster (e.g., c(2, 3, 7, 11, 12))
djg_cluster_c_names <- rownames(clusters)[which(clusters$Cluster_3 == 1)] #vector of numerical IDs representing each country that are part of the same cluster (e.g., c(6, 8, 13, 14))

djg_cluster_a <- which(clusters$Cluster_1 == 1) #vector of numerical IDs representing each country that are part of the same cluster (e.g., c(1, 4, 5, 9, 10))
djg_cluster_b <- which(clusters$Cluster_2 == 1) #vector of numerical IDs representing each country that are part of the same cluster (e.g., c(2, 3, 7, 11, 12))
djg_cluster_c <- which(clusters$Cluster_3 == 1) 

djg_clusters_list <- list(cluster_a = djg_cluster_a, 
                          cluster_b = djg_cluster_b, 
                          cluster_c = djg_cluster_c)

djg_clusters_invariance <- list()
# Loop through each cluster in the list
for (i in seq_along(djg_clusters_list)) {
  cluster_name <- names(djg_clusters_list)[i]
  cluster <- djg_clusters_list[[i]]
  
  # Keep participants who belong to the cluster of interest, removing rows with missing values
  temp_data <- data_djg[data_djg$country %in% cluster, ]
  temp_data <- na.omit(temp_data)
  temp_config <- cfa(model = djg_model_2F,
                     data = temp_data,
                     estimator = "WLSMV",
                     group = "country")
  
  temp_metric <- cfa(model = djg_model_2F,
                     data = temp_data,
                     estimator = "WLSMV",
                     group = "country",
                     group.equal = c("loadings"))
  temp_metric_sign <- lavTestLRT(temp_config, temp_metric)
  
  temp_scalar <- cfa(model = djg_model_2F,
                     data = temp_data,
                     estimator = "WLSMV",
                     group = "country",
                     group.equal = c("loadings", "intercepts"))
  temp_scalar_sign <- lavTestLRT(temp_metric, temp_scalar)
  
  # Add the results in a nested list under the cluster name
  djg_clusters_invariance[[cluster_name]] <- list(
    configural = fitmeasures(temp_config, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea")),
    metric = list(model = fitmeasures(temp_metric, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea")), sign = temp_metric_sign %$% c("chisq" = .$`Chisq diff`[2], "df" = .$`Df diff`[2], "p" = .$`Pr(>Chisq)`[2])),
    scalar = list(model = fitmeasures(temp_scalar, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea")), sign = temp_scalar_sign  %$% c("chisq" = .$`Chisq diff`[2], "df" = .$`Df diff`[2], "p" = .$`Pr(>Chisq)`[2]))
  )
}

djg_clusters_invariance

# Initialize a dataframe to store the results
results_df <- data.frame(clusterID = character(),
                         chisq_configural = character(),
                         cfi_configural = numeric(),
                         rmsea_configural = numeric(),
                         chisq_metric = character(),
                         chisq_metric_diff = character(),
                         chisq_scalar = character(),
                         chisq_scalar_diff = character(),
                         stringsAsFactors = FALSE)

# Function to format chisq, df, and pvalue into a string with specified rounding and conditions
format_chisq <- function(chisq, df, pvalue) {
  pvalue_str <- ifelse(pvalue < 0.001, "<.001", sprintf("%.2e", pvalue))
  sprintf("%.0f (%.0f, %s)", chisq, df, pvalue_str) # Chisq rounded to 0 decimals
}

# Function to round values to two decimals
round_two_decimals <- function(value) {
  round(value, 2)
}

clusters <- c("cluster_a", "cluster_b", "cluster_c")

for (cluster in clusters) {
  configural <- djg_clusters_invariance[[cluster]]$configural
  metric_model <- djg_clusters_invariance[[cluster]]$metric$model
  metric_sign <- djg_clusters_invariance[[cluster]]$metric$sign
  scalar_model <- djg_clusters_invariance[[cluster]]$scalar$model
  scalar_sign <- djg_clusters_invariance[[cluster]]$scalar$sign
  
  # Extract and format the information with rounding
  chisq_configural <- format_chisq(configural["chisq"], configural["df"], configural["pvalue"])
  cfi_configural <- round_two_decimals(configural["cfi"])
  rmsea_configural <- round_two_decimals(configural["rmsea"])
  chisq_metric <- format_chisq(metric_model["chisq"], metric_model["df"], metric_model["pvalue"])
  chisq_metric_diff <- format_chisq(metric_sign["chisq"], metric_sign["df"], metric_sign["p"])
  chisq_scalar <- format_chisq(scalar_model["chisq"], scalar_model["df"], scalar_model["pvalue"])
  chisq_scalar_diff <- format_chisq(scalar_sign["chisq"], scalar_sign["df"], scalar_sign["p"])
  
  # Append the row to the dataframe
  results_df <- rbind(results_df, data.frame(clusterID = cluster,
                                             chisq_configural = chisq_configural,
                                             cfi_configural = cfi_configural,
                                             rmsea_configural = rmsea_configural,
                                             chisq_metric = chisq_metric,
                                             chisq_metric_diff = chisq_metric_diff,
                                             chisq_scalar = chisq_scalar,
                                             chisq_scalar_diff = chisq_scalar_diff,
                                             stringsAsFactors = FALSE))
}

# View the results
print(results_df)

# save the results to a CSV file
write.csv(results_df, "clusters_invariance.csv", row.names = FALSE)

#----------------
#T-ILS
#----------------

#testing for measurement invariance (configural; metric; scalar) using multigroup confirmatory factor analysis
data_tils <- subset(data, select = c(country, loneliness_ucla_a, loneliness_ucla_b, loneliness_ucla_c))
data_tils <- na.omit(data_tils)

tils_config <- cfa(model = tils_model,
                   data = data_tils,
                   ordered = TRUE,
                   estimator = "WLSMV",
                   group = "country")

tils_metric <- cfa(model = tils_model,
                   data = data_tils,
                   ordered = TRUE,
                   estimator = "WLSMV",
                   group = "country",
                   group.equal = c("loadings"))
fitmeasures(tils_metric, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

tils_scalar <- cfa(model = tils_model,
                   data = data_tils,
                   ordered = TRUE,
                   estimator = "WLSMV",
                   group = "country",
                   group.equal = c("loadings", "intercepts"))
fitmeasures(tils_scalar, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
tils_sign_scalar <- lavTestLRT(tils_metric, tils_scalar)


# Social support ----------------------------------------------------------
data_social_support <- subset(data, select = c(social_support_a, social_support_b, social_support_c, social_support_d))
data_social_support <- na.omit(data_social_support)
fit_social_support_model <- cfa(model = 'social_suppport =~ social_support_a + social_support_b + social_support_c + social_support_d', 
                                data = data_social_support,
                                estimator = "MLR") #Maximum Likelihood (ML) methods work fine with measures answered on a 5-point Likert type scale. Here we apply the Robust Maximum Likelihood (MLR) method as it is robust to the violation of multivariate normality, an assumption that undermines the use of the ML method and that barely holds with such measures.
social_support_ic <- semTools::reliability(fit_social_support_model)

#----------------------------------------------------
#CONSTRUCT VALIDITY (Nomological Networks analyses)
#----------------------------------------------------

# djg_emot_mean, djg_social_mean - higher score, more lonely
# tils_mean - higher score, more lonely
# loneliness_direct - higher score, more lonely

# feelings_depr, feelings_happy, and neighbours - higher scores indicate greater depression, greater happiness, and greater contacts with neighbours
# health_general - higher scores indicate better subjective health
# "social_support_mean", "family_meet_face", "family_meet_tele", "friends_meet_face", "friends_meet_tele", "family_n__1__open", "friends_n__1__open", "social_activities_a"

# We expect positive correlations between the primary measures (djg_emot_mean, tils_mean, loneliness_direct) and feelings_depr
# We expect negative correlations between the primary measures (djg_emot_mean, tils_mean, loneliness_direct) and the following secondary measures: "social_support_mean", "family_meet_face", "family_meet_tele", "friends_meet_face", "friends_meet_tele", "family_n__1__open", "friends_n__1__open", "social_activities_a", "neighbours", "feelings_happy", "health_general"

#----------------
#First, we compute the factor score of composite measures (i.e., DJGLS-6, T-ILS, social support)
#----------------
# data <- dataOrg
# Define models for each construct
models <- list(
  djg_emot = 'djg_emot =~ loneliness_djg_a + loneliness_djg_b + loneliness_djg_c',
  djg_social = 'djg_social =~ loneliness_djg_d + loneliness_djg_e + loneliness_djg_f',
  tils = 'tils =~ loneliness_ucla_a + loneliness_ucla_b + loneliness_ucla_c',
  social_support = 'social_support =~ social_support_a + social_support_b + social_support_c + social_support_d'
)

# Manually specify relevant variables for each construct to avoid any mismatches
relevant_vars_per_construct <- list(
  djg_emot = c("loneliness_djg_a", "loneliness_djg_b", "loneliness_djg_c"),
  djg_social = c("loneliness_djg_d", "loneliness_djg_e", "loneliness_djg_f"),
  tils = c("loneliness_ucla_a", "loneliness_ucla_b", "loneliness_ucla_c"),
  social_support = c("social_support_a", "social_support_b", "social_support_c", "social_support_d")
)

# Loop through each construct, fit the CFA model, and store factor scores
for (construct in names(models)) {
  # Filter data for current construct, ensuring all specified variables are complete and have variance
  temp_data <- na.omit(data[relevant_vars_per_construct[[construct]]])
  if(nrow(temp_data) > 0) {
    fit <- cfa(models[[construct]], data = temp_data, estimator = "MLR")
    # Use the original 'data' dataframe for storing factor scores to preserve dataset dimensions
    # Factor scores will be NA for rows/observations removed by na.omit
    data[[paste0(construct, "_lv")]] <- NA  # Initialize the column with NAs
    valid_indices <- which(complete.cases(data[relevant_vars_per_construct[[construct]]]))
    data[valid_indices, paste0(construct, "_lv")] <- lavPredict(fit, type = "lv")[,1]
  } else {
    warning(paste("No valid data for construct:", construct))
  }
}

# Add single-indicator constructs to the models and relevant_vars_per_construct lists
single_indicators <- c("loneliness_direct",  "health_general", "feelings_depr", "feelings_happy", 
                       "family_meet_face", "family_meet_tele", "friends_meet_face", "friends_meet_tele", 
                       "family_n__1__open", "friends_n__1__open", "neighbours", "social_activities_a")

# For single indicators, define a CFA model for each, assuming a loading of .70
for (indicator in single_indicators) {
  models[indicator] <- sprintf('%s =~ .7*%s', paste0(indicator, "_lv"), indicator)
  relevant_vars_per_construct[indicator] <- c(indicator)
}

# Loop through each new single-indicator construct to compute and store factor scores
for (construct in names(models)) {
  # Filter data for current construct, ensuring specified variables are complete and have variance
  temp_data <- na.omit(data[, relevant_vars_per_construct[[construct]]])
  
  if(nrow(temp_data) > 0) {
    fit <- cfa(models[[construct]], data = temp_data, estimator = "WLSMV", ordered = TRUE)  # fixed.x = FALSE is not needed for WLSMV
    # Initialize factor score columns for single-indicator constructs with NA
    data[[paste0(construct, "_fs")]] <- NA  
    valid_indices <- which(complete.cases(data[, relevant_vars_per_construct[[construct]]]))
    # Extract factor scores for valid indices
    data[valid_indices, paste0(construct, "_fs")] <- lavPredict(fit, type = "lv")[,1]
  } else {
    warning(paste("No valid data for construct:", construct))
  }
}

#----------------
#Second, we compute the correlation coefficients for the nomological network, for each country and for each measure
#----------------
variableList <- list(
  fullNet = c("social_support_fs", "health_general_fs", "feelings_depr_fs", "feelings_happy_fs", "family_meet_face_fs", "family_meet_tele_fs", "friends_meet_face_fs", "friends_meet_tele_fs", "family_n__1__open_fs", "friends_n__1__open_fs", "neighbours_fs", "social_activities_a_fs"),
  deprHappy = c("feelings_depr_fs", "feelings_happy_fs"),
  socialActivities = c("social_support_fs", "family_meet_face_fs", "family_meet_tele_fs", "friends_meet_face_fs", "friends_meet_tele_fs", "family_n__1__open_fs", "friends_n__1__open_fs", "neighbours_fs", "social_activities_a_fs"),
  health = c("health_general_fs")
)

# Initialize a list to store results for each variable set
results_list <- list()

# Initialize a list to store frequency tables for each variable set
freq_tables_list <- list()

for (set_name in names(variableList)) {
  variables <- variableList[[set_name]]
  
  # New: Initialize a list for frequency tables of the current variable set
  freq_tables <- list()
  
  # Iterate over the variables to create and store their frequency tables
  for (variable in variables) {
    # Store frequency table in the list instead of printing
    freq_tables[[variable]] <- table(data[[variable]], useNA = "ifany")
  }
  
  # Store the frequency tables list for the current set
  freq_tables_list[[set_name]] <- freq_tables
  
  # Initialize the list for storing network per country for the current variable set
  nom_network_per_country <- list()
  for (country in countries) {
    country_list <- list()
    for (primary_measure in c("djg_emot_fs", "djg_social_fs", "tils_fs", "loneliness_direct_fs")) {
      primary_measure_list <- list()
      
      for (secondary_measure in variables) {
        primary_measure_list[[secondary_measure]] <- NA
      }
      country_list[[primary_measure]] <- primary_measure_list
    }
    nom_network_per_country[[country]] <- country_list
  }
  
  # Computing the correlations for the current variable set
  for (country in countries) {
    temp_data <- data[data$country_chr == country, ]
    
    for (primary_measure in c("djg_emot_fs", "djg_social_fs", "tils_fs", "loneliness_direct_fs")) {
      for (secondary_measure in variables) {
        
        # Ensure measures are numeric
        if(is.numeric(temp_data[[primary_measure]]) && is.numeric(temp_data[[secondary_measure]])) {
          valid_data_indices <- complete.cases(temp_data[, c(primary_measure, secondary_measure)])
          
          if (sum(valid_data_indices) > 0) {
            valid_data <- temp_data[valid_data_indices, ]
            
            # Compute Pearson correlation
            pearson_cor <- cor(valid_data[[primary_measure]], valid_data[[secondary_measure]], use = "complete.obs")
            
            # Fit linear model and compute summary
            linear_model_formula <- as.formula(paste(secondary_measure, "~", primary_measure))
            linear_model <- lm(linear_model_formula, data = valid_data)
            summary_linear_model <- summary(linear_model)
            p_value <- summary_linear_model$coefficients[2, 4]
            
            # Store results
            nom_network_per_country[[country]][[primary_measure]][[secondary_measure]] <- list(p_value = p_value, pearson_cor = pearson_cor)
          } else {
            cat(sprintf("Insufficient valid data for primary measure: %s or secondary measure: %s in country: %s\n", primary_measure, secondary_measure, country))
          }
        } else {
          cat(sprintf("Data for primary measure: %s or secondary measure: %s in country: %s is not numeric\n", primary_measure, secondary_measure, country))
        }
      }
    }
  }
  
  # Initialize an empty data frame to store the results for the current variable set
  results_df <- data.frame(country = character(), 
                           primary_measure = character(), 
                           secondary_measure = character(), 
                           p_value = numeric(), 
                           pearson_cor = numeric(), 
                           stringsAsFactors = FALSE)
  
  # Loop through each level of the nested list structure for the current variable set
  for (country in names(nom_network_per_country)) {
    for (primary_measure in names(nom_network_per_country[[country]])) {
      for (secondary_measure in names(nom_network_per_country[[country]][[primary_measure]])) {
        # Extract the list containing p_value and pearson_cor
        measure_data <- nom_network_per_country[[country]][[primary_measure]][[secondary_measure]]
        if(is.list(measure_data) && !is.null(measure_data[['p_value']]) && !is.null(measure_data[['pearson_cor']])) {
          p_value <- measure_data[['p_value']]
          pearson_cor <- measure_data[['pearson_cor']]
        } else {
          p_value <- NA
          pearson_cor <- NA
        }
        
        # Append to the results data frame
        results_df <- rbind(results_df, data.frame(country, primary_measure, secondary_measure, pearson_cor, p_value, stringsAsFactors = FALSE))
      }
    }
  }
  
  # Apply rounding to all numeric columns in the dataframe
  results_df <- data.frame(lapply(results_df, function(x) if(is.numeric(x)) round(x, 3) else x))
  
  # Store the processed dataframe in the results_list under the current set name
  results_list[[set_name]] <- results_df
}

# Assuming results_list is available and contains all results segmented by variableList subsets
# Adjustments are necessary here to iterate over each segment in results_list if needed

# Define the measures and their expected direction
measure_sets <- list(
  djg = c("djg_emot_fs", "djg_social_fs"),
  tils = "tils_fs",
  loneliness = "loneliness_direct_fs"
)

expected_direction <- list(
  positive = "feelings_depr_fs",
  negative = c("social_support_mean_fs", "family_meet_face_fs", "family_meet_tele_fs", 
               "friends_meet_face_fs", "friends_meet_tele_fs", "family_n__1__open_fs", 
               "friends_n__1__open_fs", "social_activities_a_fs", "neighbours_fs", 
               "feelings_happy_fs", "health_general_fs")
)

# Function to determine if correlation is in the expected direction and meets criteria
is_cor_meeting_criteria <- function(cor, measure, p_value, expected_positive, expected_negative) {
  if (p_value <= 0.004 && abs(cor) > 0.10) {
    if (measure %in% expected_positive && cor > 0) {
      return(TRUE)
    } else if (measure %in% expected_negative && cor < 0) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Adapt the loop to work with results from each set in variableList
proportions_list <- list()

for (set_name in names(variableList)) {
  results_df <- results_list[[set_name]]  # Retrieve the results dataframe for the current set
  
  # Following your original analysis but scoped within the current set of variables
  for (measure_set_name in names(measure_sets)) {
    measure_group <- measure_sets[[measure_set_name]]
    
    filtered_results <- results_df %>%
      filter(primary_measure %in% measure_group) %>%
      mutate(meets_criteria = mapply(is_cor_meeting_criteria, pearson_cor, secondary_measure, p_value, 
                                     MoreArgs = list(expected_positive = expected_direction$positive, 
                                                     expected_negative = expected_direction$negative)))
    
    proportions <- filtered_results %>%
      group_by(country) %>%
      summarize(proportion = mean(meets_criteria), .groups = 'drop')
    
    proportions_list[[paste(set_name, measure_set_name, sep = "_")]] <- proportions
  }
}

# Combine and process all proportions for output
combined_proportions_df <- bind_rows(lapply(names(proportions_list), function(name) {
  set_df <- proportions_list[[name]]
  set_df$measure_set <- name
  set_df
}), .id = "measure_set_id")

# Output the combined dataframe to an Excel file
write.xlsx(combined_proportions_df, "nomologicalNetCorr.xlsx")

final_results <- data.frame(
  Measure_Set = character(),
  Proportion_Over_Two_Thirds = numeric(),
  Count_Over_Two_Thirds = integer(),
  stringsAsFactors = FALSE
)

for (name in names(proportions_list)) {
  current_table <- proportions_list[[name]]
  
  proportion_over_two_thirds <- mean(current_table$proportion > 2/3)
  count_over_two_thirds <- sum(current_table$proportion > 2/3)
  
  final_results <- rbind(final_results, data.frame(
    Measure_Set = name,
    Proportion_Over_Two_Thirds = proportion_over_two_thirds,
    Count_Over_Two_Thirds = count_over_two_thirds
  ))
}

print(final_results)
















#############################
#############################
# GRAVEYARD
#############################
variables <- c("social_support_fs", "health_general_fs", "feelings_depr_fs", "feelings_happy_fs", "family_meet_face_fs", "family_meet_tele_fs", "friends_meet_face_fs", "friends_meet_tele_fs", "family_n__1__open_fs", "friends_n__1__open_fs", "neighbours_fs", "social_activities_a_fs")

# Iterate over the variables and print their frequency tables
for (variable in variables) {
  cat("Frequency table for", variable, ":\n")
  print(table(data[[variable]], useNA = "ifany")) # useNA = "ifany" will include NA values in the table if they exist
  cat("\n") # Just for better readability in the output
}

#here we generate the list in which we will store all the results
nom_network_per_country <- list()
for(country in countries){
  country_list <- list()
  for (primary_measure in c("djg_emot_fs", "djg_social_fs", "tils_fs", "loneliness_direct_fs")) {
    primary_measure_list <- list()
    
    for (secondary_measure in c("social_support_fs", "health_general_fs", "feelings_depr_fs", "feelings_happy_fs", "family_meet_face_fs", "family_meet_tele_fs", "friends_meet_face_fs", "friends_meet_tele_fs", "family_n__1__open_fs", "friends_n__1__open_fs", "neighbours_fs", "social_activities_a_fs")) {
      primary_measure_list[[secondary_measure]] <- NA
    }
    country_list[[primary_measure]] <- primary_measure_list
  }
  nom_network_per_country[[country]] <- country_list
}

# here, we compute all the correlations
for (country in countries) {
  temp_data <- data[data$country_chr == country, ]
  
  for (primary_measure in c("djg_emot_fs", "djg_social_fs", "tils_fs", "loneliness_direct_fs")) {
    for (secondary_measure in c("social_support_fs", "health_general_fs", "feelings_depr_fs", "feelings_happy_fs", "family_meet_face_fs", "family_meet_tele_fs", "friends_meet_face_fs", "friends_meet_tele_fs", "family_n__1__open_fs", "friends_n__1__open_fs", "neighbours_fs", "social_activities_a_fs")) {
      
      # Ensure measures are numeric
      if(is.numeric(temp_data[[primary_measure]]) && is.numeric(temp_data[[secondary_measure]])) {
        valid_data_indices <- complete.cases(temp_data[, c(primary_measure, secondary_measure)])
        
        if (sum(valid_data_indices) > 0) {
          valid_data <- temp_data[valid_data_indices, ]
          
          # Compute Pearson correlation
          pearson_cor <- cor(valid_data[[primary_measure]], valid_data[[secondary_measure]], use = "complete.obs")
          
          # Fit linear model and compute summary
          linear_model_formula <- as.formula(paste(secondary_measure, "~", primary_measure))
          linear_model <- lm(linear_model_formula, data = valid_data, na.action = na.exclude)
          summary_linear_model <- summary(linear_model)
          p_value <- summary_linear_model$coefficients[2, 4]
          
          # Store results
          nom_network_per_country[[country]][[primary_measure]][[secondary_measure]] <- list(p_value = p_value, pearson_cor = pearson_cor)
        } else {
          cat(sprintf("Insufficient valid data for primary measure: %s or secondary measure: %s in country: %s\n", primary_measure, secondary_measure, country))
        }
      } else {
        cat(sprintf("Data for primary measure: %s or secondary measure: %s in country: %s is not numeric\n", primary_measure, secondary_measure, country))
      }
    }
  }
}

# Initialize an empty data frame to store the results
results_df <- data.frame(country = character(), 
                         primary_measure = character(), 
                         secondary_measure = character(), 
                         p_value = numeric(), 
                         pearson_cor = numeric(), 
                         stringsAsFactors = FALSE)

# Loop through each level of the nested list structure
for (country in names(nom_network_per_country)) {
  for (primary_measure in names(nom_network_per_country[[country]])) {
    for (secondary_measure in names(nom_network_per_country[[country]][[primary_measure]])) {
      # Extract the list containing p_value and pearson_cor
      measure_data <- nom_network_per_country[[country]][[primary_measure]][[secondary_measure]]
      if(is.list(measure_data) && !is.null(measure_data[['p_value']]) && !is.null(measure_data[['pearson_cor']])) {
        p_value <- measure_data[['p_value']]
        pearson_cor <- measure_data[['pearson_cor']]
      } else {
        p_value <- NA
        pearson_cor <- NA
      }
      
      # Append to the results data frame
      results_df <- rbind(results_df, data.frame(country, primary_measure, secondary_measure, pearson_cor, p_value, stringsAsFactors = FALSE))
    }
  }
}

# Apply rounding to all numeric columns in the dataframe
results_df["pearson_cor"] <- round(results_df["pearson_cor"], 3)
results_df["p_value"] <- round(results_df["p_value"], 3)

# Alternatively, to round all numeric columns automatically
results_df <- data.frame(lapply(results_df, function(x) if(is.numeric(x)) round(x, 3) else x))

# Display the updated dataframe
print(results_df)

# Define the measures and their expected direction
measure_sets <- list(
  djg = c("djg_emot_fs", "djg_social_fs"),
  tils = "tils_fs",
  loneliness = "loneliness_direct_fs"
)

expected_direction <- list(
  positive = "feelings_depr_fs",
  negative = c("social_support_mean_fs", "family_meet_face_fs", "family_meet_tele_fs", 
               "friends_meet_face_fs", "friends_meet_tele_fs", "family_n__1__open_fs", 
               "friends_n__1__open_fs", "social_activities_a_fs", "neighbours_fs", 
               "feelings_happy_fs", "health_general_fs")
)

# Initialize a list to store the proportions for each measure set
proportions_list <- list()

# Function to determine if correlation is in the expected direction and meets criteria
is_cor_meeting_criteria <- function(cor, measure, p_value, expected_positive, expected_negative) {
  if (p_value <= 0.004 && abs(cor) > 0.10) {
    if (measure %in% expected_positive && cor > 0) {
      return(TRUE)
    } else if (measure %in% expected_negative && cor < 0) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Loop through each measure set
for (set_name in names(measure_sets)) {
  measure_group <- measure_sets[[set_name]]
  
  # Filter the results for the current set of measures
  filtered_results <- results_df %>%
    filter(primary_measure %in% measure_group) %>%
    mutate(meets_criteria = mapply(is_cor_meeting_criteria, pearson_cor, secondary_measure, p_value, 
                                   MoreArgs = list(expected_positive = expected_direction$positive, 
                                                   expected_negative = expected_direction$negative)))
  
  # Compute proportions for each country
  proportions <- filtered_results %>%
    group_by(country) %>%
    summarize(proportion = mean(meets_criteria), .groups = 'drop')
  
  # Store the computed proportions
  proportions_list[[set_name]] <- proportions
}

proportions_list$djg
proportions_list$tils
proportions_list$loneliness

# Convert the proportions list into a single dataframe with an identifier for each measure set
proportions_df <- bind_rows(
  lapply(names(proportions_list), function(set_name) {
    set_df <- proportions_list[[set_name]]
    set_df$measure_set <- set_name # Add a column to identify the measure set
    set_df
  }),
  .id = "measure_set_id" # This adds an additional column if needed to differentiate rows by their original list element names
)

# Print the combined dataframe
openxlsx::write.xlsx(proportions_df, "nomologicalNetCorr.xlsx")

# Initialize a dataframe to store the final proportions and counts
final_results <- data.frame(
  Measure_Set = character(),
  Proportion_Over_Two_Thirds = numeric(),
  Count_Over_Two_Thirds = integer(),
  stringsAsFactors = FALSE
)

# Loop through each proportions table
for (set_name in names(proportions_list)) {
  # Extract the current proportions table
  current_table <- proportions_list[[set_name]]
  
  # Calculate the proportion of countries where the proportion of significant correlations is greater than 2/3
  proportion_over_two_thirds <- mean(current_table$proportion > 2/3)
  
  # Count the number of countries where the proportion of significant correlations is greater than 2/3
  count_over_two_thirds <- sum(current_table$proportion > 2/3)
  
  # Append the results to the final_results dataframe
  final_results <- rbind(final_results, data.frame(
    Measure_Set = set_name,
    Proportion_Over_Two_Thirds = proportion_over_two_thirds,
    Count_Over_Two_Thirds = count_over_two_thirds
  ))
}

# To view the final results
print(final_results)


##########################

# Assuming 'data' and 'countries' are defined elsewhere in your script

variableList <- list(
  fullNet = c("social_support_fs", "health_general_fs", "feelings_depr_fs", "feelings_happy_fs", "family_meet_face_fs", "family_meet_tele_fs", "friends_meet_face_fs", "friends_meet_tele_fs", "family_n__1__open_fs", "friends_n__1__open_fs", "neighbours_fs", "social_activities_a_fs"),
  deprHappy = c("feelings_depr_fs", "feelings_happy_fs"),
  socialActivities = c("social_support_fs", "family_meet_face_fs", "family_meet_tele_fs", "friends_meet_face_fs", "friends_meet_tele_fs", "family_n__1__open_fs", "friends_n__1__open_fs", "neighbours_fs", "social_activities_a_fs"),
  health = c("health_general_fs")
)

# Initialize a list to store results for each variable set
results_list <- list()

# Initialize a list to store frequency tables for each variable set
freq_tables_list <- list()

for (set_name in names(variableList)) {
  variables <- variableList[[set_name]]
  
  # New: Initialize a list for frequency tables of the current variable set
  freq_tables <- list()
  
  # Iterate over the variables to create and store their frequency tables
  for (variable in variables) {
    # Store frequency table in the list instead of printing
    freq_tables[[variable]] <- table(data[[variable]], useNA = "ifany")
  }
  
  # Store the frequency tables list for the current set
  freq_tables_list[[set_name]] <- freq_tables
  
  # Initialize the list for storing network per country for the current variable set
  nom_network_per_country <- list()
  for (country in countries) {
    country_list <- list()
    for (primary_measure in c("djg_emot_fs", "djg_social_fs", "tils_fs", "loneliness_direct_fs")) {
      primary_measure_list <- list()
      
      for (secondary_measure in variables) {
        primary_measure_list[[secondary_measure]] <- NA
      }
      country_list[[primary_measure]] <- primary_measure_list
    }
    nom_network_per_country[[country]] <- country_list
  }
  
  # Computing the correlations for the current variable set
  for (country in countries) {
    temp_data <- data[data$country_chr == country, ]
    
    for (primary_measure in c("djg_emot_fs", "djg_social_fs", "tils_fs", "loneliness_direct_fs")) {
      for (secondary_measure in variables) {
        
        # Ensure measures are numeric
        if(is.numeric(temp_data[[primary_measure]]) && is.numeric(temp_data[[secondary_measure]])) {
          valid_data_indices <- complete.cases(temp_data[, c(primary_measure, secondary_measure)])
          
          if (sum(valid_data_indices) > 0) {
            valid_data <- temp_data[valid_data_indices, ]
            
            # Compute Pearson correlation
            pearson_cor <- cor(valid_data[[primary_measure]], valid_data[[secondary_measure]], use = "complete.obs")
            
            # Fit linear model and compute summary
            linear_model_formula <- as.formula(paste(secondary_measure, "~", primary_measure))
            linear_model <- lm(linear_model_formula, data = valid_data)
            summary_linear_model <- summary(linear_model)
            p_value <- summary_linear_model$coefficients[2, 4]
            
            # Store results
            nom_network_per_country[[country]][[primary_measure]][[secondary_measure]] <- list(p_value = p_value, pearson_cor = pearson_cor)
          } else {
            cat(sprintf("Insufficient valid data for primary measure: %s or secondary measure: %s in country: %s\n", primary_measure, secondary_measure, country))
          }
        } else {
          cat(sprintf("Data for primary measure: %s or secondary measure: %s in country: %s is not numeric\n", primary_measure, secondary_measure, country))
        }
      }
    }
  }
  
  # Initialize an empty data frame to store the results for the current variable set
  results_df <- data.frame(country = character(), 
                           primary_measure = character(), 
                           secondary_measure = character(), 
                           p_value = numeric(), 
                           pearson_cor = numeric(), 
                           stringsAsFactors = FALSE)
  
  # Loop through each level of the nested list structure for the current variable set
  for (country in names(nom_network_per_country)) {
    for (primary_measure in names(nom_network_per_country[[country]])) {
      for (secondary_measure in names(nom_network_per_country[[country]][[primary_measure]])) {
        # Extract the list containing p_value and pearson_cor
        measure_data <- nom_network_per_country[[country]][[primary_measure]][[secondary_measure]]
        if(is.list(measure_data) && !is.null(measure_data[['p_value']]) && !is.null(measure_data[['pearson_cor']])) {
          p_value <- measure_data[['p_value']]
          pearson_cor <- measure_data[['pearson_cor']]
        } else {
          p_value <- NA
          pearson_cor <- NA
        }
        
        # Append to the results data frame
        results_df <- rbind(results_df, data.frame(country, primary_measure, secondary_measure, pearson_cor, p_value, stringsAsFactors = FALSE))
      }
    }
  }
  
  # Apply rounding to all numeric columns in the dataframe
  results_df <- data.frame(lapply(results_df, function(x) if(is.numeric(x)) round(x, 3) else x))
  
  # Store the processed dataframe in the results_list under the current set name
  results_list[[set_name]] <- results_df
}

# Example to view results for 'fullNet'
print(results_list[["fullNet"]])

# Assuming results_list is available and contains all results segmented by variableList subsets
# Adjustments are necessary here to iterate over each segment in results_list if needed

# Adapt the loop to work with results from each set in variableList
proportions_list <- list()

for (set_name in names(variableList)) {
  results_df <- results_list[[set_name]]  # Retrieve the results dataframe for the current set
  
  # Following your original analysis but scoped within the current set of variables
  for (measure_set_name in names(measure_sets)) {
    measure_group <- measure_sets[[measure_set_name]]
    
    filtered_results <- results_df %>%
      filter(primary_measure %in% measure_group) %>%
      mutate(meets_criteria = mapply(is_cor_meeting_criteria, pearson_cor, secondary_measure, p_value, 
                                     MoreArgs = list(expected_positive = expected_direction$positive, 
                                                     expected_negative = expected_direction$negative)))
    
    proportions <- filtered_results %>%
      group_by(country) %>%
      summarize(proportion = mean(meets_criteria), .groups = 'drop')
    
    proportions_list[[paste(set_name, measure_set_name, sep = "_")]] <- proportions
  }
}

# Combine and process all proportions for output
combined_proportions_df <- bind_rows(lapply(names(proportions_list), function(name) {
  set_df <- proportions_list[[name]]
  set_df$measure_set <- name
  set_df
}), .id = "measure_set_id")

# Output the combined dataframe to an Excel file
write.xlsx(combined_proportions_df, "nomologicalNetCorr.xlsx")

final_results <- data.frame(
  Measure_Set = character(),
  Proportion_Over_Two_Thirds = numeric(),
  Count_Over_Two_Thirds = integer(),
  stringsAsFactors = FALSE
)

for (name in names(proportions_list)) {
  current_table <- proportions_list[[name]]
  
  proportion_over_two_thirds <- mean(current_table$proportion > 2/3)
  count_over_two_thirds <- sum(current_table$proportion > 2/3)
  
  final_results <- rbind(final_results, data.frame(
    Measure_Set = name,
    Proportion_Over_Two_Thirds = proportion_over_two_thirds,
    Count_Over_Two_Thirds = count_over_two_thirds
  ))
}

print(final_results)
