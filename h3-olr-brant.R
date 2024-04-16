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
feature_cols <- c('F001_01', 'F001_02', 'F001_03', 'F001_04', 'F001_05')
et_cols <- c('ET01_01', 'ET01_12')

df$EP <- rowMeans(df[,ep_cols], na.rm = TRUE) %>% round(0)
df$ET <- rowMeans(df[,et_cols], na.rm = TRUE) %>% round(0)
df$Features <- rowMeans(df[,feature_cols], na.rm = TRUE) %>% round(0)
df$Bereitschaft_Offline <- rowMeans(df[,bereitschaft_cols], na.rm = TRUE) %>% round(0)

df$Features <- factor(df$Features, ordered = TRUE)
df$EP <- factor(df$EP, ordered = TRUE)
df$ET <- factor(df$ET, ordered = TRUE)
df$Bereitschaft_Offline <- factor(df$Bereitschaft_Offline, ordered = TRUE)

# Fit an Ordinal Logistic Regression Model
#3.1 F0 -> B0
formula_F0_B0 <- paste("Bereitschaft_Offline ~", paste(feature_cols, collapse = " + "))
olr_model_F0_B0_brant <- polr(as.formula(formula_F0_B0), data = df, Hess=TRUE)
print(brant(olr_model_F0_B0_brant))

olr_model_F0_B0 <- clm(as.formula(formula_F0_B0), data=df)
print(summary(olr_model_F0_B0))

#3.2 F0 -> EP
formula_F0_EP <- "EP ~ F001_01 + F001_02 + F001_03 + F001_04 + F001_05"
olr_model_F0_EP_brant <- polr(as.formula(formula_F0_EP), data = df, Hess=TRUE)
print(brant(olr_model_F0_EP_brant))

olr_model_F0_EP <- clm(as.formula(formula_F0_EP), data = df)
print(summary(olr_model_F0_EP))

#3.3 F0 + EP -> B0
formula_combined <- "Bereitschaft_Offline ~  EP01_01 + EP01_02 + EP01_03 + EP01_06 + F001_01 + F001_02 + F001_03 + F001_04 + F001_05"
olr_model_combined_brant <- polr(as.formula(formula_combined), data = df, Hess=TRUE)
print(brant(olr_model_combined_brant))
# => ET01_11 does not satisfy the parallel regression assumption

olr_model_combined <- clm(as.formula(formula_combined), data=df)
print(summary(olr_model_combined))

# get coefficients
coefficients_combined <- summary(olr_model_combined)$coefficients
# calculate odds ratios
odds_ratio_combined <- exp(coefficients_combined[ ,"Estimate"])
# bind back to coefficients
(coefficients_combined <- cbind(coefficients_combined, odds_ratio_combined))


# Wahrscheinlichkeiten...
# Vorhersage der Klassenwahrscheinlichkeiten für jede Beobachtung
predicted_probabilities_3 <- fitted(olr_model_combined_brant)

write.csv(predicted_probabilities_3, file="h3-fitted-prob.csv")

