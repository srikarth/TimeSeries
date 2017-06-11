library(readr)
library(fpp)
library(TTR)
Final_Travel <- read.csv("/Users/Srinivas/Desktop/Final_Travel.csv")
travel<- Final_Travel$Value
#plot(travel,type = "l")

travel_ts <- ts(travel,start = c(1999,1),frequency = 12)
plot(travel_ts)
#Time series plot of International travel to United States between the year 1999 to 2016.
#We can see that from time series that there seems to be seasonal variation in the number of visitors every year.
#There is peak every alternate year.It can be described using an additive model,as the seasonal fluctuations are roughly constant in size over time and do not seem to depend on the level of the time series,
#and the random fluctuations also seem to be roughly constant in size over time.There is significant drop towards the end of 2001 because of the 9/11 attack
travel_ts_2 <- window(travel_ts,start = c(2002,1))
plot(travel_ts_2)

#the plot is from 2002 to 2016 removing the drop which occured due to 9/11 incident 

summary(travel_ts_2)
boxplot(travel_ts_2)
##The box plot indicates that the number of International Travel is between 1197000 and 2569000 with an median of 2056000.The number of tourists visits varies in this range.Also the mean and
#median are not much separated which gives an idea that roughly around 50% of values are less than mean and 50% values are greater than mean.
#There are also some outliers beyond 4075000 which suggests that there are some values in the dataset that do no fit in the possible range of datapoints.






travel_decomp <- stl(travel_ts_2,s.window = "periodic")
plot(travel_decomp)
#Yes the time Series is seasonal
summary(travel_decomp)
travel_decomp_1 <-decompose(travel_ts_2) 
travel_decomp_1$type
#Time series is additive in nature. At a particular point in time series the value is the summation of all the three components of the time series.
seasonal_indices <- travel_decomp_1$seasonal
seasonal_indices
barplot(seasonal_indices[1:12],xlab="Months",ylab = "Number of Visitors",axisnames = TRUE)
max_indices <- which(seasonal_indices==max(seasonal_indices))
max_indices
# July
min_indices <- which(seasonal_indices==min(seasonal_indices))
min_indices
# February
#For the month of July time series value is high and for the month of February the value of time series is low.
#generally during the month of July ,The climate is much pleasant compared to other months which may be a reason for people to travel .During that time lot of International students come to US for Education which may be another reason for the value being high during that month.

plot(travel_ts_2)
lines(seasadj(travel_decomp_1),col="red")
# Yes seasonality has a major impact on the time series.Plot for seasonal adjusted time series is almost a straight line with a positive slope.





# Naive
naive_forecast <- naive(travel_ts_2,12)
naive_forecast
summary(naive_forecast)
plot(naive_forecast)

plot(naive_forecast$residuals,type="p")
#residuals are random over the years.
hist(naive_forecast$residuals)
#It is normally distributed with skewness towards the left
plot(as.numeric(naive_forecast$fitted),as.numeric(naive_forecast$residuals),type="p",xlab="Fitted",ylab="Residuals")
#residuals are random and do not depend on fitted values
plot(as.numeric(naive_forecast$x),as.numeric(naive_forecast$residuals),type="p",xlab="Actual",ylab="Residuals")
#residuals are random and independant from the actual values.
Acf(naive_forecast$residuals)
#There is seasonal variation in the acf and justifies the plot in the original time series
summary(naive_forecast)
accuracy(naive_forecast)
#accuracy measure for naive

#seasonalnaive
travel_naive <- snaive(travel_ts_2,h=12)
plot(travel_naive)
plot(travel_naive$residuals,type="p")
#The plot indicates that the residuals are random over the years.
hist(travel_naive$residuals)
#The histogram plot confirms that the residuals are random and follow a normal distribution with mean at 0.
plot(as.numeric(travel_naive$fitted),as.numeric(travel_naive$residuals),type="p",xlab="Fitted",ylab="Residuals")
#The plot shows that the residuals are random and do not depend on fitted values.
plot(as.numeric(travel_naive$x),as.numeric(travel_naive$residuals),type="p",xlab="Actual",ylab="Residuals")
#The plot shows the randomness of residuals and independence from the actual values.
Acf(travel_naive$residuals)
#The Acf plot shows that there is there is residuals at the starting of the time series but is significant later on which implies that residuals are not related to each other.

accuracy(travel_naive)
summary(travel_naive)
plot(travel_naive)

#By comparing the RMSE between  naive and seasonal naive .The RMSE is better in seasonal and has good accuracy.
#The prediction for the next year is almost same as the previous year values,due to some fluctuations the plot shows that 95% times the values will be in shaded blue region.


# Simple moving average
plot(travel_ts_2,ylab="Number of Visitors")
travel_ma3 <- ma(travel_ts_2,order = 3)
lines(travel_ma3,col="red")
travel_ma6 <- ma(travel_ts_2,order = 6)
lines(travel_ma6,col="blue")
travel_ma12 <- ma(travel_ts_2,order = 12)
lines(travel_ma12,col="green")
#The choice of order for moving average is as inferred from the plot above as we go on increasing the order we are actually removing the seasonality. One can easily compare the above plot where green line is almost same as the plot for seasonal adjusted time series.
travel_ma1 <- ma(travel_ts_2,order = 1)
forecast_ma<-forecast(travel_ma1,h=12)
plot(forecast_ma)






# Simple Smoothing
travel_ss <- ets(travel_ts_2)
#ss_summary <- travel_ss$par
forecast.ets(travel_ss)
summary(travel_ss)
#alpha = 0.4147 .Equal weights are given to the previous value to determine the forecasted value.
#sigma:  0.0469 signifies that residuals have a standard deviation of this value.

#Initial states:
#l = 1615326.4368 
#s=1.0256 0.8825 1.0596 1.0823 1.2087 1.2262
#       1.0286 1.0079 0.972 0.9245 0.7721 0.81

#ss_summary <- travel_ss$par
plot(travel_ss$residuals,type="p",ylab="Residuals")
#Residuals are random and distributed
hist(travel_ss$residuals,xlab = "Residuals",main="Histogram of Residuals")
#The plot confirms that the residuals are random with a normal distribution with a mean of 0.
plot(as.numeric(travel_ss$fitted),travel_ss$residuals,type="p",xlab="Fitted",ylab="Residuals")
#residuals are independant of fitted values
plot(as.numeric(travel_ss$x),travel_ss$residuals,type="p",xlab="Actual",ylab="Residuals")
#residuals are random and are independant of actual values
Acf(travel_ss$residuals)
#Acf shows spike at 6 .residuals are not significant.
accuracy(travel_ss)
forecast_ss <- forecast.ets(travel_ss,h=12)
forecast_ss
plot(forecast_ss)
accuracy(forecast_ss)

#MAPE(mean absolute percentage error) is 3.4% which is very low and hence has an accuracy of 96.6% which is  good and can be used to forecast values for the next year.
#The predicted values from September 2016 to August 2017 is repetitive of the previous period. The residuals are very less and the number of visitors will be same as the previous year as the model takes into account more recent values. Also the model shows the area and confirms that the 95% of values will lie in the shaded region.




#HoltWinters
travel_holts <- HoltWinters(travel_ts_2)
travel_holts$alpha
#The Value of alpha signifies that more weight very recent observations in the time series.
travel_holts$beta
#The value of Beta indicates there is  not much trend.
travel_holts$gamma
#Gamma is seasonal component of forecast.The higher the value the most recent seasonal component is weighed
travel_holts$coefficients

#From the values of alpha, beta and gamma it is clear that for our forecast recent values are weighed more heavily.

plot(residuals(travel_holts),type="p",ylab="Residuals")
#There is a constant variance in the plot
hist(residuals(travel_holts),xlab = "Residuals",main="Histogram of Residuals")
#The histogram is normally distributed
Acf(residuals(travel_holts))
#there is spike in 6 and 12 .The residuals are not significant.

plot(as.numeric(travel_holts$fitted[,1]),as.numeric(residuals(travel_holts)),type="p",
     xlab="Fitted",ylab="Residuals")

#There are no patterns found
#plot(as.numeric(travel_holts$x[13:164]),as.numeric(residuals(travel_holts)),type="p",xlab="Actual",ylab="Residuals")


forecast_holts <- forecast.HoltWinters(travel_holts,h=12)
summary(forecast_holts)
#The accuracy is good
plot(forecast_holts)
accuracy(forecast_holts)

#Predicted Values in one year
forecast_holts

# ARIMA
# No the time series is not stationary. It can be easily viewed by seeing the original
# time series. However we will confirm by following tests.

adf.test(travel_ts_2)
# p  value is < 0.05 and hence no differencing required ,data is not stationary.
kpss.test(travel_ts_2)
# p value is <0.05 and hence differencing required
# test confirms that time series is not stationary and hence require differecing.

nsdiffs(travel_ts_2)
# 1 differencing required
tsdisplay(travel_ts_2)
#The plot is seasonal .
fit <- stl(travel_ts_2, s.window=5)
plot(fit)
plot(seasadj(fit))


#diffrencing function
travel_ts_diff1 <- diff(travel_ts_2, differences=1)
tsdisplay(travel_ts_diff1)
ndiffs(travel_ts_diff1)
adf.test(travel_ts_diff1)

#Hence we don't need any more differences.
#The data is non-stationary now

Acf(travel_ts_diff1, lag.max=20)
Pacf(travel_ts_diff1, lag.max=20)

#auto.arima(travel_ts)
#Based on the ACF and PACF plots we take ARIMA models as (0,1,1) and (2,1,2)

auto.fit1<-Arima(travel_ts_diff1,c(0,1,1))
auto.fit2<-Arima(travel_ts_diff1,c(2,1,2))
summary(auto.fit1)
summary(auto.fit2)

# AIC
auto.fit1$aic
auto.fit2$aic

# BIC
auto.fit1$bic
auto.fit2$bic

# Sigma^2
auto.fit1$sigma2
auto.fit2$sigma2
#From above observations we find that auto.fit2 model is better based on the AIC and BIC values.
#So, we consider ARIMA(2,1,2) model.

#The fitted model formula is: (1 + 1.0038B)(1 + 0.2462B^2) X_t = (1 - 0.1932B)(1 - 0.8068B^2)
#Residual Analysis
#Plot of residuals
plot(auto.fit2$residuals)
#Resdiuals do not follow a particular pattern.


#Histogram of residuals
hist(auto.fit2$residuals)
#The histogram  is normally distributed



#Plot of actual vs residuals
plot(as.numeric(travel_ts_diff1), as.numeric(auto.fit2$residuals),type="p",xlab= "Actual",ylab="Residuals")

#There are  patterns found

#Acf plot of residuals
Acf(auto.fit2$residuals)

#There is  autocorrelation and so the forecasting technique is not so good
travel_forecast_arima<-forecast.Arima(auto.fit2,h=12)

accuracy(travel_forecast_arima)


#Forecast 
travel_forecast_arima

plot(travel_forecast_arima)

#Summarize the forecasting technique
summary(travel_forecast_arima)

#The accuracy is not that good compared to other methods.




#Accuracy Summary


#Naive
accuracy(naive_forecast)

#Seasonal Naive
accuracy(travel_naive)

#Simple Smoothing
accuracy(forecast_ss)

#HoltWInters
accuracy(forecast_holts)

#ARIMA
accuracy(travel_forecast_arima)

#Definition
#(Simple Smoothing)
#Time series that can be described using an additive model with constant level and no seasonality, you can use simple exponential smoothing to make short-term forecasts.The simple exponential smoothing method provides a way of estimating the level at the current time point. 
#Smoothing is controlled by the parameter alpha; for the estimate of the level at the current time point.

#Holtwinters
# IF you have a time series that can be described using an additive model with increasing or decreasing trend and seasonality, you can use Holt-Winters exponential smoothing to make short-term forecasts.
#Holt-Winters exponential smoothing estimates the level, slope and seasonal component at the current time point. Smoothing is controlled by three parameters: alpha, beta, and gamma, for the estimates of the level

#Arima 
#While exponential smoothing methods do not make any assumptions about correlations between successive values of the time series, in some cases you can make a better predictive model by taking correlations in the data into account. 
#Autoregressive Integrated Moving Average (ARIMA) models include an explicit statistical model for the irregular component of a time series, that allows for non-zero autocorrelations in the irregular component.

#NaiveForcasting
#Estimating technique in which the last period' actuals are used as this period's forecast without adjusting them.It is used only for comparison with forecasts generated by the better techniques.

#The worst method is Naive

#Ranking the forecasting Methods considering the RMSE(root mean square error)
#1 Simple Smoothing
#2 Holt Winters
#3 Seasonal Naive
#4 Arima
#5 Naive


#Conculusion
#We can see that the data has an upward trend but also seasonality is present
#Since the trend is upward, the value of time series will increase in the next year



