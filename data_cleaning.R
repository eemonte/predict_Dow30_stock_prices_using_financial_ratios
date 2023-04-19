library(tidyquant)
library(dplyr)
library(tidyr)
library(glmnet)

rm(list = ls())

setwd("C:/Users/gugao/OneDrive - Suncor Energy Inc/Documents/Data Science/GT Courses/MGT 6203/Project")

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


df_clean[order(-df_clean$public_date),]


# Below is a test example to use Lasso Regression to remove features in the clean dataset for AAPL stock, 
AAPL <- df_clean[df_clean$TICKER == 'AAPL',]

y_AAPL <- AAPL$close
x_AAPL <- data.matrix(AAPL[,3:54])

cv_model <- cv.glmnet(x_AAPL, y_AAPL, alpha = 1)
best_lambda <- cv_model$lambda.min
plot(cv_model)

best_model <- glmnet(x_AAPL, y_AAPL, alpha = 1, lambda = best_lambda)
coef(best_model)

y_predicted <- predict(best_model, s = best_lambda, newx = x_AAPL)

#find SST and SSE
sst <- sum((y_AAPL - mean(y_AAPL))^2)
sse <- sum((y_predicted - y_AAPL)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq


# Below is a test example to use Lasso Regression to remove features in the clean dataset for all stocks, 


y <- df_clean$close
x <- data.matrix(df_clean[,3:54])

cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.min
plot(cv_model)

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq
library(jtools)
plot_summs(best_model)

# The following financial ratios are identified as having major impact on stock price from the general Dow 30 stocks
# Positive impact on stock price: opm_bd: Operating Profit Margin Before Depcreciation, npm: net profit margin, accrual: acruals/Average Assets, Total Debt/Total Assets
# Negative impact on stock price: adv_sale: Advertising Expense/Sales, roa: Return on Assets, cfm: cash flow margin, capital ratio: capitalization ratio, debt_invcap: Long-term Debt/Invested Capital (debt_invcap)
