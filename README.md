### Team-42's group project GitHub repository for MGT 6203 (Canvas) Spring of 2023 semester.

#### About our project
Understanding how to make successful investments is important for both investment businesses and individuals, as effective allocation of capital is extremely important for their success and financial health. In this project, we first analyzed the relationship between financial ratios and stock prices by using data from the Dow 30 stocks. After experimenting several models, we decided to train a Random Forest model to generate top-5 most important ratios for predicting a stock’s performance. Finally, we concluded certain financial ratios are highly correlated to particular stocks. Therefore, we can make more informed investment decisions and potentially generate higher returns on our investments by implementing factor analysis to predict future stock performance.

### Data
We used 2 datasets for this project:
1. From the [Wharton Research Data Services](https://wrds-www.wharton.upenn.edu/), we obtained a dataset Dow30FinancialRatios.csv including the Dow 30 stocks' 75 financial ratios by month from January 2010 to December 2022. You can access this dataset [***here***](https://www.dropbox.com/s/g74sv12p39q15do/Dow30FinancialRatios.csv?dl=0).
2. Using R tidyquant, we obtained a second dataset stock_prices.csv, which includes the Yahoo Finance Dow 30 stock prices by month from January 2010 to December 2022. You can access this dataset [***here***](https://www.dropbox.com/s/we2ed1i1t98cin9/stock_prices.csv?dl=0).

### GitHub repository
In this repository, we stored the project deliverables into the following 7 folders:
1. Project Proposal: ***team42proposal.pdf***
2. Progress Report: ***team42progressreport.pdf***
3. Proposal Presentation: ***team42proposalpresentation.pdf***
4. Final Report: ***team42finalreport.pdf***
5. Final Presentation Slides: ***team42finalpresentation.pdf***
6. Code (contains archived code files):
 - ***data_cleaning.R***: cleaned and pre-processed the original datasets.
 - ***initial_model_PCA.Rmd***: tested if PCA is a good model for our problem.
 - ***initial_model_Lasso_Regression.R***: tested if Lasso Regression is a good model for our problem, also compared the Lasso Regression model with the Random Forest model.
 - ***experiment_Clustering***: experimented if we can cluster the result from the Random Forest model. However, it is not as good as we expected. As the result, we are not including this clustering model in our final result. 
7. Final Code (contains code files for the final report):
 - ***final_model_Random_Forest.R***: trained our cleaned data with a Random Forest model and discovered it is the best performed model. Therefore, we used this model for the subsequent analysis in our project.
 - ***data_cleaning_visual.R***: created visualizations displaying the data cleaning process.
8. Data (contains intermediate results):
 - ***Dow30_top_5_importance_clusters***: results generated from the Random Forest model, which later used as input for the Clustering step
 - ***Dow30_top_5_importance_clusters_input***: used in experiment_Clustering code, to cluster findings from Random Forest model  
 - ***Clustering_result***: results from the experimented Clustering step
9. Visualizations: visualization figures in the final report.
