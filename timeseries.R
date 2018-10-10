library(tseries)
library(xts)
library(urca)
require(urca)
require(tseries)
library(forecast)
require(forecast)
require(xts)
library(astsa)
require(astsa)


data<-data.frame(UKARIMA)
str(data)
date<-data$Month.of.Year
values<-data[, -1]
values
#convert to time series

datts<-ts(values,start=c(2009,1),frequency=12)
datts
str(datts)


#split to test and train
subset.ts <- function(data, start, end) {
  ks <- which(time(data) >= start & time(data) < end)
  vec <- data[ks]
  ts(vec, start=start(data) + c(0, ks[1] - 1), frequency=frequency(data))
}

train<-subset.ts(datts,2009, 2015)
test<-subset.ts(datts,2015,2016)


#plot the time serieplot(AirPassengers)
#plot(train)
#abline(reg=lm(train~time(train)))


##1st transformation using difference Yt' =Yt-T(t-1)
#plot(diff(train), ylab="differenced data")
#2nd transformation logYt=Log10(Yt)
#plot(log10(train),ylab="log transformation")
# log and then diff
#plot(diff(log10(train)), main="log and then diff train data")

#diff and then log
#plot(log10(diff(train)),main="diff and then log10 train data")

#plot acf and pacf 
#par(mfrow=c(1,2))
#acf(train,main="no differenced train data ACF")
#pacf(train,main="no differenced train data PACF")

#acf(ts(diff(log10(train))), main="ACF train data")
#pacf(ts(diff(log10(train))), main="PACF train data")
#acf(ts(diff(train)), main="ACF train data")
#pacf(ts(diff(train)), main="PACF train data")


#fit a sarima model with seasonal effect of 12 month

sarima4<-sarima(train,1,0,1,0,1,1,4)
sarima.for(train,12,1,0,1,0,1,1,4)
sarima12<-sarima(train,1,1,1,0,1,1,12)
#this is how to predict with sarima it will also product the relative graphs as well
sarima.for(train,12,1,0,1,0,1,1,12)

#use arima (p,d,q) to fit the data
ARIMA_log<-auto.arima(log10(train), approximation=FALSE,trace=FALSE)
summary(ARIMA_log)

pred_arima_log<-forecast(ARIMA.log, h=24)
pred_arima_log
ARIMA.diff<-auto.arima(diff(train), approximation=FALSE,trace=FALSE)
summary(ARIMA.diff)


#plot(pred_arima_diff)
#fit a forecasting model 
datts_ets=ets(datts)
forecast_log<-forecast(datts_ets, h=24)
forecast_diff<-forecast(datts_ets, h=24)
forecast_log
plot(forecast_log)
forecast_diff
plot(forecast_diff)


# fit the TBATs model
dat_tbats<-tbats(datts)
pred_tbats<-forecast(dat_tbats, h=24)
pred_tbats
plot(pred_tbats)

# compare different model using AIC
par(mfrow=c(1,1))
barplot(c(TBATS=dat_tbats$aic , ARIMA=ARIMA_log$aic, SARIMA=sarima12$aic), col="light blue", ylab="AIC")
barplot(c(ETS=datts_ets$bic,ARIMA=ARIMA.log$bic, ARIMA2=ARIMA.diff$bic), col="light blue", ylab="BIC")
barplot(c(ETS=datts_ets$aicc,ARIMA=ARIMA.log$aicc, ARIMA2=ARIMA.diff$aicc), col="light blue", ylab="AICC")

#plot the residuals of ACF and PACF to check if the fit is correctly done
acf(ts(ARIMA_log$residuals), main="ACF residual")
pacf(ts(ARIMA_log$residuals),main="PACF residual")


#last dates to mearsure performance
last_date=index(datts)[length(datts)]
forecast_df=data.frame(sessions_predicted=pred_arima_log$mean,
                       sessions_lower=pred_arima_log$lower[,2],
                       sessions_upper=pred_arima_log$upper[,2],
                       data=last_date+seq(1/12,2, by=1/12))
forecast_df

