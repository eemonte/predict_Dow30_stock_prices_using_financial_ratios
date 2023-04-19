library(tidyquant)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(reshape2)

rm(list = ls())
Dow30_FinancialRatios <- read.table('../Desktop/dow30.txt', sep=',', header=TRUE)

# Prepare monthly financial ratios for all 30 Dow stocks from 2010 to 2022.
#Dow30_FinancialRatios <- read.csv(file = 'Dow30FinancialRatios.csv')

Dow30_FinancialRatios <- Dow30_FinancialRatios[,5:77] # Keep only Ticker, Date and financial ratios 

Dow30_FinancialRatios <- transform(Dow30_FinancialRatios, public_date = as.Date(as.character(public_date), "%Y%m%d"))

# Prepare stock price history for all 30 Dow stocks 
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

#### Visualizations for Data Cleaning Process

## 1. highlight the impact of removing financial ratios with >500 NAs on the dataset

# Calculate percentage of missing values before and after removing NA values
before_clean <- merged %>% 
  summarise(pct_missing = round(mean(is.na(.))*100, 2)) %>% 
  pull(pct_missing)

merged_cleaned <- merged %>% 
  drop_na(close)

merged_cleaned_new <- subset(merged_cleaned, 
                             select=-c(pretret_noa,pretret_earnat,invt_act,rect_act,curr_debt,profit_lct,ocf_lct,cash_ratio,quick_ratio,curr_ratio,cash_conversion,inv_turn,sale_nwc,PEG_trailing))

after_clean <- merged_cleaned_new %>% 
  summarise(pct_missing = round(mean(is.na(.))*100, 2)) %>% 
  pull(pct_missing)

# Create data frame with missing values before and after cleaning
clean_na_df <- data.frame(
  status = c("Before cleaning", "After cleaning"),
  pct_missing = c(before_clean, after_clean))

clean_na_df$status <- factor(clean_na_df$status, levels = c("Before cleaning", "After cleaning"))

ggplot(clean_na_df, aes(x = status, y = pct_missing, fill = status)) + 
  geom_col() + 
  geom_text(aes(label = paste0(pct_missing, "%")), 
            position = position_dodge(width = 1), 
            vjust = -0.5) + 
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) + 
  labs(title = "Percentage of Missing Values Before and After Cleaning", 
       x = "Data Cleaning Status", 
       y = "Percentage of Missing Values") +
  theme_minimal()



#### 2. Evaluate the effectiveness of the imputation method and identify any outliers or unusual patterns in the imputed values.

# Imputing remaining financial ratios with NA values. 
merged_cleaned_new_imputed <- merged_cleaned_new %>% 
  group_by(TICKER) %>% 
  fill(names(merged_cleaned_new), .direction = "downup")

clean_na_df_new <- subset(merged_cleaned_new_imputed, 
                          select=-c(lt_ppent,intcov,intcov_ratio,int_debt, int_totdebt))


before_imputed <- merged_cleaned_new %>% 
  summarise(pct_missing = round(mean(is.na(.))*100,2)) %>% 
  pull(pct_missing)

after_imputed <- merged_cleaned_new_imputed %>% 
  summarise(pct_missing = round(mean(is.na(.))*100,2)) %>% 
  pull(pct_missing)

imputed_na_df <- data.frame(
  status = c("Before imputation", "After imputation"),
  pct_missing = c(before_imputed, after_imputed))

imputed_na_df$status <- factor(imputed_na_df$status, 
                               levels = c("Before imputation", "After imputation"))

ggplot(imputed_na_df, aes(x = status, y = pct_missing, fill=status)) +
  geom_col() + 
  geom_text(aes(label = paste0(pct_missing, "%")), 
            position = position_dodge(width = 1), 
            vjust = -0.5) + 
  scale_fill_manual(values = c("#ADD8E6", "#FFA500")) + 
  labs(x = "", y = "Percentage of missing values") +
  ggtitle("Comparison of missing values before and after imputation")
