################# Dollar Exchange Rate Prediction using R #################

##Importing Required Packages
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(pdfetch)

##Importing Dataset from Finance Websites...(Default yahoo)
INR = pdfetch_YAHOO("INR=X")
INR <- na.omit(INR)
View(INR)
chartSeries(INR, subset = 'last 6 months', type = 'auto')
##from the plot we can say that the data is not stationary 
first_diffs= 
##Assigning columns of dataset  
Open_prices = INR[,1]
High_prices = INR[,2]
Low_prices = INR[,3]
Close_prices = INR[, 4]
Volume_prices = INR[,6]
Adjusted_prices = INR[,5]

par(mfrow = c(2,3))

plot(Open_prices, main = 'Opening Price of Exchange rate (Over a given period)')
plot(High_prices, main = 'Highest Price of Exchange rate (Over a given period)')
plot(Low_prices, main = 'Lowest Price of Exchange rate (Over a given period)')
plot(Close_prices, main = 'Closing Price of Exchange rate (Over a given period)')
plot(Volume_prices, main = 'Volume of Exchange rate (Over a given period)')
plot(Adjusted_prices, main = 'Adjusted Price of Exchange rate (Over a given period)')

Predic_Price = Adjusted_prices
class(Predic_Price)

######## Finding the Linear Relation between observations ########

par(mfrow = c(1,2))
Acf(Predic_Price, main = 'ACF for differenced Series')
Pacf(Predic_Price, main = 'PACF for differenced Series ', col = '#cc0000')
Auto_cf = Acf(Predic_Price, plot = FALSE)
Auto_cf
PAuto_cf = Pacf(Predic_Price, plot = FALSE)
PAuto_cf
par(mfrow = c(1,2))
Acf(diff(log(Predic_Price)))
Pacf(diff(log(Predic_Price)))
print(adf.test(Predic_Price))

################### Prediction of Return ##########################

return_INR <- Predic_Price
par(mfrow = c(1,2))
plot(Predic_Price, main = 'Actual Predict_Price')
plot(return_INR, main = "Converting into stationary data")

INR_return_train <- return_INR[1:(0.9*length(return_INR))]
  
INR_return_test <- return_INR[(0.9*length(return_INR)+1):length(return_INR)]

auto.arima(INR_return_train, seasonal = FALSE)

fit <- Arima(INR_return_train, order = c(2,2,3))
checkresiduals(fit)
preds <- predict(fit, n.ahead = (length(return_INR) - (0.9*length(return_INR))))$pred
preds

################## Forecasting Predicted Result ##################

test_forecast <- forecast(fit,h = 15)
test_forecast

par(mfrow = c(1,1))
plot(test_forecast, main = "Arima forecast for Dollar rupee Exchange Rate")
accuracy(preds, INR_return_test)