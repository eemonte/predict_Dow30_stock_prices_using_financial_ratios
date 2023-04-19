# install.packages("glmnet")

library(tidyquant)
library(dplyr)
library(tidyr)
library(glmnet)

rm(list = ls())

# Prepare monthly financial ratios for all 30 Dow stocks from 2010 to 2022.
Dow30_FinancialRatios <- read.csv(file = 'Dow30FinancialRatios.csv')

Dow30_FinancialRatios <- Dow30_FinancialRatios[,5:77] # Keep only Ticker, Date and financial ratios 

Dow30_FinancialRatios <- transform(Dow30_FinancialRatios, public_date = as.Date(as.character(public_date), "%Y%m%d"))


# Prepare stock price history for all 30 Dow stocks from 2010 to 2022
stock_prices  <- tq_get(c("AAPL","AMGN", "AXP","BA","C","CAT","CRM","CSCO","CVX","DIS",
                          "DOW","GS","HD","HON","IBM","INTC","JNJ","JPM","KO","MCD",
                          "MMM","MRK","MSFT","NKE","PG","TRV","UNH","V","VZ","WBA","WMT"),
                        get  = "stock.prices",
                        from = "2010-01-01",
                        to   = "2022-12-31")

stock_prices <- as.data.frame(stock_prices)

# Join 2 dataframes 
merged <- merge(Dow30_FinancialRatios, stock_prices,
                by.x=c("TICKER","public_date"),
                by.y=c("symbol","date"),
                all.x = TRUE)

# Remove rows with NA stock price (close price) 
merged_close_na_removed <- merged %>% drop_na(close)

# Check number of NAs for each of the financial ratios. 
merged_close_na_removed %>% summarise(across(everything(), ~ sum(is.na(.))))

# Removing financial ratios with >500 NAs, which translates to about ~15% of each financial ratio's 3255 total observations. 
# Removing these financial ratios is deemed to be more beneficial than imputing them. 

merged_close_ratios_na_removed <- subset(merged_close_na_removed, select=-c(pretret_noa,pretret_earnat,invt_act,rect_act,curr_debt,profit_lct,ocf_lct,cash_ratio,quick_ratio,curr_ratio,cash_conversion,inv_turn,sale_nwc,PEG_trailing))

merged_close_ratios_na_removed %>% summarise(across(everything(), ~ sum(is.na(.))))

# Imputing remaining financial ratios with NA values. 
merged_close_ratios_na_removed_imputed <- merged_close_ratios_na_removed %>% 
  group_by(TICKER) %>% 
  fill(names(merged_close_ratios_na_removed), .direction = "downup")

# Checking NA value again
# Finding JPM is missing values for the entire duration for intcov, intcov_ratio, int_debt, int_totdebt 
# while TRV is missing values for the entire duration for lt_ppent.
# As a result, these 5 financial ratios above are removed. 

sum(is.na(merged_close_ratios_na_removed_imputed))
t <- merged_close_ratios_na_removed_imputed %>% summarise(across(everything(), ~ sum(is.na(.))))

df_clean <- subset(merged_close_ratios_na_removed_imputed, select=-c(lt_ppent,intcov,intcov_ratio,int_debt, int_totdebt))



# Below is a test example to use Lasso Regression to remove features in the clean dataset for AAPL stock, 
AAPL <- df_clean[df_clean$TICKER == 'AAPL',]

y_AAPL <- AAPL$close
x_AAPL <- data.matrix(AAPL[,3:54])

cv_model <- cv.glmnet(x_AAPL, y_AAPL, alpha = 1)
best_lambda <- cv_model$lambda.min
plot(cv_model)

best_model <- glmnet(x_AAPL, y_AAPL, alpha = 1, lambda = best_lambda)
coef(best_model)


# Random forest model

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)

dataset = df_clean[, c(3:51, 58)]

# apply min-max normalization to the close column
max_close = max(dataset$close)
min_close = min(dataset$close)

dataset$close = (dataset$close - min_close) / (max(dataset$close) - min_close)


split = sample.split(dataset$close, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
library(magrittr)
library(dplyr)

# build model
rf_model = randomForest(close ~ ., data = training_set, ntree = 500, importance = TRUE)

# Make predictions on the test set using the trained model
rf_preds = predict(rf_model, test_set)

# Calculate the mean squared error of the predictions
mse = mean((rf_preds - test_set$close)^2)

# Print the mean squared error
print(paste0("Mean Squared Error for the Random Forest: ", mse))

# Calculate R-squared
R2 = 1 - sum((test_set$close - rf_preds)^2) / sum((test_set$close - mean(test_set$close))^2)
print(paste0("R squared for the Random Forest: ", R2))

importance(rf_model)

# %IncMSE: This measure calculates the increase in mean squared error (MSE) of the model when a particular predictor is permuted randomly. 
# The higher the %IncMSE value for a predictor, the more important it is in the model.

# IncNodePurity: This measure is similar to %IncMSE, but is based on the purity of the nodes in the tree. It calculates the total reduction 
# in the sum of squares due to splits on a particular predictor. The higher the IncNodePurity value for a predictor, the more important it is in the model.

#install.packages("caret")
library(caret)
varImp(rf_model)
# Get variable importance measures
importance_matrix <- importance(rf_model, type = 2)

# Extract top 5 most important predictors based on %IncMSE values
importance_df = as.data.frame(importance_matrix)
top_5_features = head(arrange(importance_df, desc(IncNodePurity)), 5)

top_5_features

importance <- rf_model$importance[,"IncNodePurity"]

# Get names of features
feature_names <- names(rf_model$xlevels)[-1] # exclude the response variable

# Sort feature importances in descending order and select top 10
top_10_indices <- order(importance, decreasing = TRUE)[1:10]
top_10_importance <- importance[top_10_indices]
top_10_feature_names <- feature_names[top_10_indices]

par(mar = c(5, 8, 3, 2) + 0.1)

# Create a barplot of feature importances
barplot(top_10_importance,
        horiz = TRUE,
        main = "Top 10 Feature Importance Plot",
        xlab = "Importance",
        ylab = "",
        names.arg = top_10_feature_names,
        las = 2,
        xlim = c(0, max(top_10_importance) * 1.1),
        xpd = TRUE)


# Create scatter plot of predicted vs. actual values
plot(rf_preds, test_set$close,
     main = "Prediction vs. Actual Plot",
     xlab = "Predicted Values",
     ylab = "Actual Values")
abline(a = 0, b = 1, col = "red")


# re-run random forest model with top 5 key variables
dataset = df_clean[, c(1, 3:51, 58)]
stock_lst = unique(dataset$TICKER)

n = length(stock_lst)

for (i in 1:n){
  df = dataset[dataset$TICKER == stock_lst[i],]
  rf_model = randomForest(close ~ ptb + debt_capital + bm + dltt_be + adv_sale, data = df, ntree = 500, importance = TRUE)
  importance <- rf_model$importance[,"IncNodePurity"]
  print(stock_lst[i])
  print(importance)
}
