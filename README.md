# Road-Traffic-Fatalities-Prediction-Model-through-Time-Series-Analysis
Prediction of road casualties and evaluate the impact of transformations in Time Series Modeling and Forecasting with ARIMA using the R programming language

2. Dataset: UKDriversDeath which is a pre-loaded dataset in R.

3. In this project, several ARIMA models have been introduced. 

4.Model 1: Analyzed the time plot, ACF Plot of the Dataset, then tested stationarity by Augmented Dickey-Fuller (ADF) Test and Kwiatkowski-Phillips-Schmidt-Shin
(KPSS) Test. Then I selected the appropriate ARIMA model by evaluating the ACF & PACF plots of the stationary series.

5. Model 2: Logarithm transformation has been done on the dataset to obtain a better ARIMA model.
   
6. Model 3: Boxcox transformation has been used for Model 3. Lambda value = Optimum Lambda

7. Model 4: Auto ARIMA function of R.

8. Finally, I have evaluated the performance of these models based on AIC, AICc, BIC and RMSE, MAPE values and residual diagnostic results.

9.At Last, using the best-performed model, I have forecasted the 10 Points Ahead.
