library(MASS) # For ordinal logistic regression
library(brant) # For performing the Brant test
library(tidyverse) # for data manipulation

# Read the data
data_path_csv <- "./Anonyme Daten kompletto.csv" 
df <- read.csv(data_path_csv, sep = ";") 

likert_categories <- c('A0', 'B002', 'EP', 'ET', 'P0', 'F0') # Ordinal scaled, categorical variables
likert_category_questions <- grep(paste(likert_categories, collapse="|"), names(df), value=TRUE)

# Replace missing values with median for likert scale questions
df[likert_category_questions] <- lapply(df[likert_category_questions], function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
})

# Reverse the scale for specific columns
likert_reverse_columns <- c('A001_04', 'A001_05', 'ET01_12', 'P002_07', 'P002_08', 'P002_09')
df[likert_reverse_columns] <- lapply(df[likert_reverse_columns], function(x) 6 - x)

# Calculating the average for groups of columns and rounding the result
anonymity_cols <- c('A001_01', 'A001_02', 'A001_03', 'A001_04', 'A001_05')
bereitschaft_cols <- c('B002_01', 'B002_02')
safety_cols <- c('ET01_10', 'EP01_02', 'ET01_03', 'ET01_11')

df$Anonymität <- rowMeans(df[,anonymity_cols], na.rm = TRUE) %>% round(0)
df$Sicherheit <- rowMeans(df[,safety_cols], na.rm = TRUE) %>% round(0)
df$Bereitschaft_Offline <- rowMeans(df[,bereitschaft_cols], na.rm = TRUE) %>% round(0)

df$Sicherheit <- factor(df$Sicherheit, ordered = TRUE)
df$Anonymität <- factor(df$Anonymität, ordered = TRUE)
df$Bereitschaft_Offline <- factor(df$Bereitschaft_Offline, ordered = TRUE)


# Fit an Ordinal Logistic Regression Model
# 1.1  A0 -> B0
formula_str_bereitschaft <- paste("Bereitschaft_Offline ~", paste(anonymity_cols, collapse = " + "))
olr_model_bereitschaft_brant <- polr(as.formula(formula_str_bereitschaft), data = df, Hess=TRUE)

# 1.2 A0 -> S1
formula_str_sicherheit <- paste("Sicherheit ~", paste(anonymity_cols, collapse = " + "))
olr_model_sicherheit_brant <- polr("Sicherheit ~  A001_03 + A001_04 + A001_05", data = df, Hess=TRUE)
print(summary(olr_model_sicherheit_brant))

# Perform the Brant Test
brant_test_result_sicherheit <- brant(olr_model_sicherheit_brant)
print(brant_test_result_sicherheit)
brant_test_result_bereitschaft <- brant(olr_model_bereitschaft_brant)
print(brant_test_result_bereitschaft)

# 1.3 A0 + S1 -> B0
#now use anonymity and security to predict bereitschaft
formula_str_combined <- "Bereitschaft_Offline ~  A001_01 + A001_02 + A001_03 + A001_04 + A001_05 + ET01_10 + EP01_02 + ET01_03 + ET01_11"
olr_model_combined_brant <- polr(as.formula(formula_str_combined), data=df, Hess = TRUE)
brant_test_result_combined <- brant(olr_model_combined_brant)
# ET01_11 does not satisfy the parallel regression assumption

# Fit the model using clm
olr_model_sicherheit <- clm(as.formula("Sicherheit ~ A001_03 + A001_04 + A001_05"), data = df)
olr_model_bereitschaft <- clm(as.formula(formula_str_bereitschaft), data=df)
# Summary of the model including p-values
summary(olr_model_sicherheit)
summary(olr_model_bereitschaft)

olr_model_combined <- clm(as.formula(formula_str_combined), data = df)
summary(olr_model_combined)


# get coefficients 
coefficients_safety <- summary(olr_model_sicherheit)$coefficients
# calculate odds ratios
odds_ratio_safety <- exp(coefficients_safety[ ,"Estimate"])
# bind back to coefficients
(coefficients_safety <- cbind(coefficients_safety, odds_ratio_safety))

# get coefficients 
coefficients_bereitschaft <- summary(olr_model_bereitschaft)$coefficients
# calculate odds ratios
odds_ratio_bereitschaft <- exp(coefficients_bereitschaft[ ,"Estimate"])
# bind back to coefficients
(coefficients_bereitschaft <- cbind(coefficients_bereitschaft, odds_ratio_bereitschaft))

# get coefficients
coefficients_combined <- summary(olr_model_combined)$coefficients
# calculate odds ratios
odds_ratio_combined <- exp(coefficients_combined[ ,"Estimate"])
# bind back to coefficients
(coefficients_combined <- cbind(coefficients_combined, odds_ratio_combined))


## predicted probabilities for categories of B0
predicted_probabilities_1 <- fitted(olr_model_combined_brant)

write.csv(predicted_probabilities_1, file="h1-fitted-prob.csv")
