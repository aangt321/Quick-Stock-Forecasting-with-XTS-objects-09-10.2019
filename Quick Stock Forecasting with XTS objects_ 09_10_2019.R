################################################################################
# Quick Stock Forecasting with XTS objects 09/10.2019
################################################################################
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(zoo)
install.packages("timeSeries")
install.packages("Rtools")
# Pull data from Yahoo finance SPY - SPDR S&P 500 ETF (SPY)
getSymbols('SPY' , from='2014-01-01', to ='2019-09-09' )
class(SPY) # Class is xts/zoo
# Want the days close price for each trading day (4th column) - 1) Open, 2) High, 3) Low, 4) Close, 5) Volume 6) Adjusted
SPY_Close_Prices = SPY[,4]
# Plot the data - look at the last 5 years of SPY close of day prices
plot(SPY_Close_Prices)
Class(SPY_Close_Prices) # Class is xts/zoo
# Graph the ACF and PCACF looking for identifiable lags PACF -> P ACF -> Q for custom arimas
par(mfrow=c(1,2))
Acf(SPY_Close_Prices, main='ACF for Differenced Series')
Pacf(SPY_Close_Prices, main='PACF for Differenced Series')

# Test findings on original XTS objects
# ADF test for p-value
print(adf.test(SPY_Close_Prices)) # p-value = 0.2287
auto.arima(SPY_Close_Prices, seasonal=FALSE)  # Arima 3,1,4 AIC/BIC =5990/6038

fitA = auto.arima(SPY_Close_Prices, seasonal=FALSE) # auto arima (3,1,4)
tsdisplay(residuals(fitA), lag.max=40, main='(3,1,4) Model Residuals')
auto.arima(SPY_Close_Prices, seasonal=FALSE) # AIC/BIC =5990/6038

fitB = arima(SPY_Close_Prices, order=c(1,2,4)) # custom arima (1,2,4) lag at
tsdisplay(residuals(fitB), lag.max=40, main='(1,2,4) Model Residuals')

fitC = arima(SPY_Close_Prices, order=c(5,1,4)) # guess arima (5,1,4) lag based at auto arima
tsdisplay(residuals(fitC), lag.max=40, main='(5,1,4) Model Residuals')

fitD = arima(SPY_Close_Prices, order=c(1,1,1)) # default (standard defacto) arima (1,1,1) lag at auto default
tsdisplay(residuals(fitD), lag.max=40, main='(1,1,1) Model Residuals')

#Plots of arima models
par(mfrow=c(2,2))
# auto arima(2,0,2)
term<-100
fcast1 <-forecast(fitA, h=term)
plot(fcast1)
#custom arima (3,0,3) 
fcast2 <- forecast(fitB, h=term)
plot(fcast2)
fcast3 <- forecast(fitC, h=term)
plot(fcast3)
fcast4 <- forecast(fitD, h=term)
plot(fcast4)

#Mape (Mean Accuracy of % Error) accuracy subtract from 100
accuracy(fcast1)   # 99.42% accuracy
accuracy(fcast2)   # 99.41% accuracy
accuracy(fcast3)   # 99.41% accuracy
accuracy(fcast4)   # 99.42% accuracy

###########################################################################################################################################################################################################################################
forecast(fitA, h=term) # a lot of things can change for instance: rank/rating, tariff changes, terror acts, using daily data but trying to forecast longer period to get a general upward directional trend and that is what we have here.#
###########################################################################################################################################################################################################################################
###########################################################################################################################################################################################################################################

#2) Log Residuals to remove non-stationary properties:
# Compute the log returns for the stock - makes more stable
logs = diff(log(SPY_Close_Prices),lag=1)
logs = logs[!is.na(logs)]

# Plot log returns for more accurate forecasting - eliminates
par(mfrow=c(1,1))
plot(logs, type='l', main ='log returns plot')
#ADF test for p-values
print(adf.test(logs))  #p-value < 0.01

auto.arima(logs, seasonal=FALSE)  #AIC/BIC = -9598/-9577
str(logs)   #is xts object

# Split the dataset in two parts - 80/20 training and testing
sample_size = floor(0.80 * nrow(logs))
set.seed(109)  # Random seed number that when reused makes this reproducible
train_indices <- sample(seq_len(nrow(logs)), size = sample_size)

train <- logs[train_indices, ]
test  <- logs[-train.indices, ]  # "-train" means "not train"

par(mfrow=c(1,2))
Acf(train, main= 'ACF for Differenced Series')
Pacf(train, main= 'PACF for Differenced Series')
auto.arima(train, seasonal=FALSE)     #pdq = 1,0,2 AIC/BIC = 7680/7660
#from plots of ACF and PACF above we get 7,0,19 for pdq

#########################################################################################################
#########################################################################################################

#3) Plot models, get accuracy and draw conclusions:
# Look at residuals for the autoarima and custom arima based on the above determined pdq
fit1 <- auto.arima(train, seasonal = FALSE)  # auto arima (1,0,2)
tsdisplay(residuals(fit1), lag.max=40, main='(1,0,2) Model Residuals')

fit2 = arima(train, order=c(7,0,19))         # Custom arima (7,0,19)
tsdisplay(residuals(fit2), lag.max=40, main='(7,0,19) Model Residuals')

# Original data without logs of returns (lag at 31)
fit3 = auto.arima(SPY_Close_Prices, seasonal= FALSE)       # Custom arima (7,0,19)
tsdisplay(residuals(fit3), lag.max=40, main='Original, Non-Log returns Model Residuals')

# Custom Arima from fit2 above applied to original dataset
fit4 = arima(SPY_Close_Prices, order = c(7,0,19))       # Custom arima (7,0,19)
tsdisplay(residuals(fit4), lag.max=40, main='(7,0,19) Model Residuals on original dataset')

# Plots of all arima models
par(mfrow=c(2,2))
# Auto arima (2,0,2)
Period <-100
fcast1 <- forecast(fit1, h=Period)
plot(fcast1)

# Custom arima (3,0,3)
fcast2 <- forcast(fit2, h=Period)
plot(fcast2)

# Origina, non-log returns data
fcast3 <- forecast(fit3, h=Period)
plot(fcast3)

# Custom Arima applied to original, non-log returns, dataset
fcast4 <- forecast(fit4, h=Period)
plot(fcast4)

# Loook closely at auto arima on original dataset
par(mfrow=c(1,2))
plot(fcast3)
plot(fcast4)

#Mape (Mean Accuracy of % Error) accuracy subtract from 100
accuracy(fcast1)   # inf. -> slight upward followed by stationary trend
accuracy(fcast2)   # inf. -> slight upward followed by stationary trend
accuracy(fcast3)   # 99.42% accuracy by MAPE Mean Average Percent Error
accuracy(fcast4)   # 99.41% accuracy

# Look at actual forecasted value on original dataset (fcast3)
#1 - autoarima:
fcast3
#2 - Custom arima with pdq values 7,0,19.
fcast4


###############################################################################################################################
# Conclusion ##################################################################################################################
###############################################################################################################################
# We have a high accuracy prediction for between a 30% increase (auto arima on original dataset)
# and a 0% increase on the custom arima. Being that the custom arima has smaller 95% and 80% intervals
# (twice the accuracy over the period) we can conclusively state that there is a very strong projection of a 5-11% gain
# over the next 100 days in the SPY ETF and overall stock market. Due to the numerous outside factors that can impact
# the stock market (Terrorist actions, oil cartel decisions (OPEC), political occurrences, weather extremes (large
# hurricanes like Katrina), Health issues such as pandemics) we cannot and should not try to predict exact prices on a daily basis
# or similar. only look at this in a directional manner.
################################################################################################################################



