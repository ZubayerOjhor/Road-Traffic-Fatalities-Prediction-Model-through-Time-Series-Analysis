# Road-Traffic-Fatalities-Prediction-Model-through-Time-Series-Analysis
Prediction of road casualties and evaluate the impact of transformations in Time Series Modeling and Forecasting with ARIMA using the R programming language
Dataset: UKDriversDeath which is a pre-loaded dataset in R.
In this project, several ARIMA models have been introduced. 
Model 1: Analyzed the time plot, ACF Plot of the Dataset, then tested stationarity by Augmented Dickey-Fuller (ADF) Test and Kwiatkowski-Phillips-Schmidt-Shin
(KPSS) Test. Then I have selected the appropriate ARIMA model by evaluating the ACF & PACF plots of the stationary series
Model 2: Logarithm transformation has been done on the dataset to obtain a better ARIMA model.
Model 3: Boxcox transformation has been used for Model 3. Lambda value = Optimum Lambda
Model 4: Auto ARIMA function of R.
Finally, I have evaluated the performance of these models based on AIC, AICc, BIC and RMSE, MAPE values and residual diagnostic results.
At Last, using the best-performed model, I have forecasted the 10 Points Ahead.
