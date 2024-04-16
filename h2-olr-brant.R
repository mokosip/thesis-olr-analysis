library(MASS) # For ordinal logistic regression
library(brant) # For performing the Brant test
library(broom)
library(ordinal)
library(tidyverse) # for data manipulation

# Read the datax
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
ep_cols <- c('EP01_01', 'EP01_02', 'EP01_03', 'EP01_06')
bereitschaft_cols <- c('B002_01', 'B002_02')
safety_cols <- c('ET01_10', 'EP01_02', 'ET01_03', 'ET01_11')

df$EP <- rowMeans(df[,ep_cols], na.rm = TRUE) %>% round(0)
df$Sicherheit <- rowMeans(df[,safety_cols], na.rm = TRUE) %>% round(0)
df$Bereitschaft_Offline <- rowMeans(df[,bereitschaft_cols], na.rm = TRUE) %>% round(0)

df$Sicherheit <- factor(df$Sicherheit, ordered = TRUE)
df$EP <- factor(df$EP, ordered = TRUE)
df$Bereitschaft_Offline <- factor(df$Bereitschaft_Offline, ordered = TRUE)
# Fit an Ordinal Logistic Regression Model
# Assuming 'response' is your ordinal response variable and 'predictors' are your predictors

#2.1 EP -> B0
formula_EP_B0 <- paste("Bereitschaft_Offline ~", paste(ep_cols, collapse = " + "))
olr_model_EP_B0_brant <- polr(as.formula(formula_EP_B0), data = df, Hess=TRUE)
print(brant(olr_model_EP_B0_brant))

olr_model_EP_B0 <- clm(as.formula(formula_EP_B0), data=df)
print(summary(olr_model_EP_B0))

#2.2. EP -> S1
formula_EP_S1 <- "Sicherheit ~ EP01_01 + EP01_03 + EP01_06 + EP01_02"
olr_model_EP_S1_brant <- polr(as.formula(formula_EP_S1), data = df, Hess=TRUE)
print(brant(olr_model_EP_S1_brant))
# => EP01_02 (vertrauenswürdigkeit) does not satisfy the parallel regression assumption

olr_model_EP_S1 <- clm(as.formula(formula_EP_S1), data=df)
print(summary(olr_model_EP_S1))

#2.3. EP + S1 -> B0 // try with and without ET01_11 for different significant coeffs
formula_combined <- "Bereitschaft_Offline ~  EP01_01 + EP01_02 + EP01_03 + EP01_06 + ET01_10 + ET01_03 + ET01_11"
olr_model_combined_brant <- polr(as.formula(formula_combined), data = df, Hess=TRUE)
print(brant(olr_model_combined_brant))
# => ET01_11 does not satisfy the parallel regression assumption

#2.3. EP + S1 -> B0 // without ET01_11 f
formula_combined_without_ET01_11 <- "Bereitschaft_Offline ~  EP01_01 + EP01_02 + EP01_03 + EP01_06 + ET01_10 + ET01_03"
olr_model_combined_brant_without_ET01_11 <- polr(as.formula(formula_combined_without_ET01_11), data = df, Hess=TRUE)
print(brant(olr_model_combined_brant_without_ET01_11))

olr_model_combined <- clm(as.formula(formula_combined), data=df)
print(summary(olr_model_combined))

olr_model_combined_without_ET01_11 <- clm(as.formula(formula_combined_without_ET01_11), data=df)
print(summary(olr_model_combined_without_ET01_11))


# get coefficients
coefficients_combined <- summary(olr_model_combined)$coefficients
coefficients_combined_without <- summary(olr_model_combined_without_ET01_11)$coefficients
# calculate odds ratios
odds_ratio_combined <- exp(coefficients_combined[ ,"Estimate"])
odds_ratio_combined_without <- exp(coefficients_combined_without[ , "Estimate"])
# bind back to coefficients
(coefficients_combined <- cbind(coefficients_combined, odds_ratio_combined))
(coefficients_combined_without <- cbind(coefficients_combined_without, odds_ratio_combined_without))


# Teste auf Multikolinearität zwischen EP01_01, EP01_06 und EP01_02
spearman_correlation <- cor(df[c("EP01_01", "EP01_06", "EP01_02")], method = "spearman")
print(spearman_correlation) 
# => keine Mutlikolinearität

# Get fitted probabilities for each category of B0
predicted_probabilities_2 <- fitted(olr_model_combined_brant)

write.csv(predicted_probabilities, file="h2-fitted-prob.csv")
