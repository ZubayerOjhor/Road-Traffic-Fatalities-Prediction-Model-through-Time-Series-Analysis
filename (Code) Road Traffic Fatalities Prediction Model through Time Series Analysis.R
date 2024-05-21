#Developed by MD. ZUBAYER
plot(UKDriverDeaths)
acf(UKDriverDeaths)
ggsubseriesplot(UKDriverDeaths)
length(UKDriverDeaths)
View(UKDriverDeaths)

#train and test split
traindata=ts(UKDriverDeaths[1:134], frequency = 12, start=c(1969,1))
length(traindata)

testdata=ts(UKDriverDeaths[135:192], frequency = 12, start=c(1980,3))
length(testdata)

#stationary check
library(tseries)
adf.test(traindata)##p-value = 0.01 stationary
kpss.test(traindata)##p-value = 0.04793, non-stationary

##taking seasonal and non seasonal differences
Dtraindata=diff(traindata, differences = 1, lag = 1)
adf.test(Dtraindata)##p-value = 0.01 stationary
kpss.test(Dtraindata)##p-value = 0.1 stationary

#Model_1
library(forecast)
tsdisplay(Dtraindata)

M1=Arima(traindata, order = c(1, 1, 0), seasonal = list(order = c(1, 1, 1), period = 12))
M1 ##AIC=1566.68   AICc=1567.03   BIC=1577.87

### Residuals Diagnostic
library(itsmr)
test(M1$residuals)

##Forecasting
n=length(testdata)
n
forecast_M1 <- forecast::forecast(M1,h=n)
plot(forecast_M1)
forecast_M1$mean
     
### Calculating Forecasting Accuracy Measures
MM1=forecast::forecast(M1, h=n)$mean
err=testdata - MM1
ME=sum(err)/n
print(ME)
MSE=sum(err^2)/n
print(MSE)
RMSE=sqrt(MSE)
print(RMSE)##176.9536
MAE=sum(abs(err))/n
print(MAE)
PE=(err/testdata)*100
MPE=sum(PE)/n
print(MPE)
MAPE=sum(abs(PE))/n
print(MAPE)

plot(traindata)

### Using Log transformation
Ltraindata=log(traindata)
plot(Ltraindata)
adf.test(Ltraindata)## P_val=0.01 stationary
kpss.test(Ltraindata)##p-value = 0.04191 non - stationary

#Taking Difference
DLtraindata=diff(Ltraindata, differences = 1, lag=1)
kpss.test(DLtraindata)##p-value = 0.1

#Model_2
library(forecast)
tsdisplay(DLtraindata)

M2=Arima(Ltraindata, order = c(2, 1, 1), seasonal = list(order = c(1, 0, 1), period = 12))
M2 ##AIC=-262.65    AICc=-261.98   BIC=-245.31

### Residuals Diagnostic
library(itsmr)
test(exp(M2$residuals))

##Forecasting
n=length(testdata)
n
forecast_M2 <- forecast::forecast(M2,h=n)
plot(forecast_M2)
#Back Transformation for RMSE 
forecast_MM2 <- forecast::forecast(M2,h=n)$mean
MM2=exp(forecast_MM2)

forecast_M2_backtransformed <- exp(forecast_M2$mean)
forecast_M2_backtransformed
round(forecast_M2_backtransformed,2)

### Calculating Forecasting Accuracy Measures
err=testdata - MM2
ME=sum(err)/n
print(ME)
MSE=sum(err^2)/n
print(MSE)
RMSE=sqrt(MSE)
print(RMSE)##192.5335
MAE=sum(abs(err))/n
print(MAE)
PE=(err/testdata)*100
MPE=sum(PE)/n
print(MPE)
MAPE=sum(abs(PE))/n
print(MAPE)

#Box-Cox Transformation
BXtraindata=BoxCox(traindata,lambda = BoxCox.lambda(traindata))
plot(BXtraindata)
acf(BXtraindata)

#stationary check
library(tseries)
adf.test(BXtraindata)##p-value = 0.01 stationary
kpss.test(BXtraindata)##p-value = 0.04726, non-stationary

##taking seasonal and non seasonal differences
DBXtraindata=diff(BXtraindata, differences = 1)

kpss.test(DBXtraindata)##p-value = 0.1 stationary
adf.test(DBXtraindata)

#Model_3
library(forecast)
tsdisplay(DBXtraindata)

M3=Arima(BXtraindata, order = c(1, 1, 1), seasonal = list(order = c(1, 0, 3), period = 12))
M3 ##AIC=1506.52    AICc=1507.42     BIC=1526.76

### Residuals Diagnostic
library(itsmr)
test(M3$residuals)

##Forecasting
forecast_M3 <- forecast::forecast(M3,h=n)
plot(forecast_M3)
forecast_M3_back <- forecast_M3
forecast_M3_back$mean <- InvBoxCox(forecast_M3$mean, lambda = BoxCox.lambda(traindata))
forecast_M3_back$mean

#Back Transformation for RMSE 
MM3 <- forecast::InvBoxCox(forecast_M3$mean, lambda = BoxCox.lambda(traindata))

### Calculating Forecasting Accuracy Measures
err=testdata - MM3
ME=sum(err)/n
print(ME)
MSE=sum(err^2)/n
print(MSE)
RMSE=sqrt(MSE)
print(RMSE)##202.9743
MAE=sum(abs(err))/n
print(MAE)
PE=(err/testdata)*100
MPE=sum(PE)/n
print(MPE)
MAPE=sum(abs(PE))/n
print(MAPE)

#Model 4: Auto Arima
M4=auto.arima(traindata)
M4
#ARIMA(0,1,1)(2,1,0)[12] AIC=1569.65   AICc=1570   BIC=1580.84

### Residuals Diagnostic
library(itsmr)
test(M4$residuals)

##Forecasting
forecast_M4 <- forecast::forecast(M4,h=n)
plot(forecast_M4)
forecast_M4$mean

### Calculating Forecasting Accuracy Measures
MM4=forecast::forecast(M4, h=n)$mean
err=testdata - MM4
ME=sum(err)/n
print(ME)
MSE=sum(err^2)/n
print(MSE)
RMSE=sqrt(MSE)
print(RMSE)##184.5308
MAE=sum(abs(err))/n
print(MAE)
PE=(err/testdata)*100
MPE=sum(PE)/n
print(MPE)
MAPE=sum(abs(PE))/n
print(MAPE)

# Train the best model on the entire dataset
RM_M1 <- Arima(UKDriverDeaths, order = c(1, 1, 0), seasonal = list(order = c(1, 1, 1), period = 12))

# Forecast 10 points ahead
forecast_result <- forecast::forecast(RM_M1,h=10)

# Plot original data and forecasted values
plot(forecast_result, main = "Forecast for UKDriverDeaths Dataset", xlab = "Year", ylab = "Deaths")
forecast_result$mean



