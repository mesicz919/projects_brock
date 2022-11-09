setwd("~/Documents/NEW MAC/Office/Brock/MBE/ECON 5P04/LAMARCHE ASSIGNMENT")

install.packages("ggplot2")
install.packages("forecast")
install.packages("fpp2")
install.packages("stargazer")
library("ggplot2")
library("forecast")
library("fpp2")
library("stargazer")

df<-read.csv("Unemployment Rate Data.csv", header = TRUE)
unrate<-df$UNRATENSA

#create time series
ts_unrate<-ts(unrate,start=c(2010,1),end=c(2020,2), frequency=12)

#Q1
autoplot(ts_unrate) + ggtitle("Civilian Monthly Unemployment Rate") +
  xlab("Month") + ylab("Unemployment Rate (in %)")
ggseasonplot(ts_unrate, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Unemployment Rate (in %)") + ggtitle("Seasonal plot: Monthly Unemployment Rate")

#Q2
ggAcf(ts_unrate, lag=10) + ggtitle("Civilian Monthly Unemployment Rate Correlogram") +
  xlab("Lag") + ylab("ACF")
unrate_auto_corr<-ggAcf(ts_unrate, plot = FALSE)
unrate_auto_corr_10<-unrate_auto_corr[1:10,]
unrate_auto_corr_10


#Q3
unrtrain<-window(ts_unrate,start=c(2010,1), end=c(2017,12)) 
unrtest<-window(ts_unrate, start=c(2018,1), end=c(2020,2))

#a) Forecasts
meanf<-meanf(unrtrain,h=26)
meanf_list<-meanf$mean
naive<-naive(unrtrain, h=26)
naive_list<-naive$mean
snaive<-snaive(unrtrain, h=26)
snaive_list<-snaive$mean

meanf_list
naive_list
snaive_list

#b) Plot forecasts
autoplot(ts_unrate) + ggtitle("Civilian Monthly Unemployment Rate") + xlab("Year") + ylab("Unemployment Rate (in %)")

autoplot(unrtrain) + autolayer(unrtest, series="Test Data") + autolayer(meanf$mean, series="Mean") +
  autolayer(naive$mean, series="Naive") + autolayer(snaive$mean, series="Seasonal Naive") + 
  ggtitle("Forecasts for Civilian Monthly Unemployment Rate") + 
  xlab("Year") + ylab("Unemployment Rate (in %)") + guides(colour=guide_legend(title="Forecasts"))

#c) Accuracy
unr_acc<-window(ts_unrate, start=2018)
accuracy(meanf, unr_acc)
accuracy(naive, unr_acc)
accuracy(snaive, unr_acc)
accuracybind<-rbind(accuracy(meanf, unr_acc), (accuracy(naive, unr_acc)), (accuracy(snaive, unr_acc)))
stargazer(accuracybind) 

#Q4
#trend
trend_reg<-tslm(unrtrain ~ trend)
summary(trend_reg)
checkresiduals(trend_reg)
#seasonal
seas_reg<-tslm(unrtrain ~ season)
summary(seas_reg)
checkresiduals(seas_reg)
#trend and seas
trend_seas_reg<-tslm(unrtrain ~ trend + season)
summary(trend_seas_reg)
checkresiduals((trend_seas_reg))

#Q5
bestmodel<-rbind(CV(trend_reg), CV(seas_reg), CV(trend_seas_reg))
bestmodel
stargazer(bestmodel)

#Q6
#trend
unr_forecast<-window(ts_unrate, start=c(2018,1), end=c(2020,2))

forecast_trend<-forecast(trend_reg, newdata = unr_forecast)
autoplot(ts_unrate, series="Data")+autolayer(forecast_trend, level = FALSE,PI = TRUE,series="Fitted") +
  xlab("Month") +ylab("Unemployment Rate (in %)") +
  ggtitle("Trend: Civilian Monthly Unemployment Rate")
#season
forecast_season<-forecast(seas_reg, newdata = unr_forecast)
autoplot(ts_unrate, series="Data")+autolayer(forecast_season, level = FALSE,PI = TRUE,series="Fitted") +
  xlab("Month") +ylab("Unemployment Rate (in %)") + ggtitle("Seasonal: Civilian Monthly Unemployment Rate")
#trend and season
forecast_trend_season<-forecast(trend_seas_reg, newdata = unr_forecast)
autoplot(ts_unrate, series="Data") +
  autolayer(forecast_trend_season, level = FALSE ,PI = TRUE ,series="Fitted") +
  xlab("Month") + ylab("Unemployment Rate (in %)") +
  ggtitle("Trend and Season: Civilian Monthly Unemployment Rate")

forecast_trend
forecast_season
forecast_trend_season

#Q7
forecast_accuracy<-rbind(accuracy(forecast_trend, unr_forecast),
                        accuracy(forecast_season, unr_forecast), accuracy(forecast_trend_season, unr_forecast))
forecast_accuracy
stargazer(forecast_accuracy)

#Q8

final_tslm<-tslm(ts_unrate ~ trend + season)
final_tslm_for<-forecast(final_tslm, h=12)
autoplot(ts_unrate) + ggtitle("Civilian Monthly Unemployment Rate") + xlab("Year") + ylab("Unemployment Rate (in %)")
autoplot(ts_unrate) + autolayer(forecast(final_tslm, h=12), series="Trend Regression Forecasts") +
ggtitle("12-Month Forecast for Civilian Monthly Unemployment Rate (Trend)") + xlab("Year") + ylab("Unemployment Rate (in %)")

final_tslm_for
