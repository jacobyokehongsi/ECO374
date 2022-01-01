setwd("D:/UofT/Y2 Summer/ECO374/Essay")

library(forecast)
library(aTSA)

data = read.csv("essaydata.csv")
data = data.frame(data$y)
data = data[complete.cases(data), ]
data_ts = ts(data,start=1,frequency = 12)
class(data_ts)
plot.ts(data_ts, 
        plot.type=c("single"), 
        col=c("blue"),
        main="Data Plot",
        ylab="y",
        xlab="t")



adf.test(data_ts) # not stationary

acf(data_ts)
Acf(data_ts)
pacf(data_ts)
Pacf(data_ts)

# needs differencing
data_ts %>% diff(lag = 12) %>% ggtsdisplay()
data_ts %>% diff(lag = 12) %>% diff() %>% ggtsdisplay()
diff_data_ts = diff(data_ts, lag = 12)
diff_data_ts = diff(diff_data_ts)


adf.test(diff_data_ts) # stationary now

acf(diff_data_ts)
Acf(diff_data_ts)
pacf(diff_data_ts)
Pacf(diff_data_ts)

arima(data_ts, order=c(2,1,3), seasonal=c(0,1,2))
arima0 = arima(data_ts, order=c(1,1,2), seasonal=c(0,1,1)) # no difference (og)

arima1 = arima(diff_data_ts, order=c(0,0,1), seasonal=c(0,0,1))
arima2 = arima(diff_data_ts, order=c(1,0,0), seasonal=c(0,0,1))
arima3 = arima(diff_data_ts, order=c(1,0,0), seasonal=c(0,0,1))
arima4 = arima(diff_data_ts, order=c(1,0,1), seasonal=c(0,0,1))
arima5 = arima(diff_data_ts, order=c(1,0,2), seasonal=c(0,0,1)) # min aic
c(AIC(arima1), AIC(arima2), AIC(arima3), AIC(arima4), AIC(arima5))

y_res = ts(arima5$residuals, start=1, frequency = 12)
y_res_og = ts(arima0$residuals, start=1, frequency = 12)

Box.test(y_res, lag=1, type="Ljung-Box")
Box.test(y_res, lag=2, type="Ljung-Box")
Box.test(y_res, lag=3, type="Ljung-Box")

Box.test(y_res_og, lag=1, type="Ljung-Box")
Box.test(y_res_og, lag=2, type="Ljung-Box")
Box.test(y_res_og, lag=3, type="Ljung-Box")

# forecasting

forecast_diff = predict(arima5, n.ahead = 12) # diff
forecast_diff
plot(forecast_diff$pred, main = 
             "12 period ahead forecasts for the differenced data")
ts.plot(diff_data_ts, forecast$pred, gpars = list(col=c("black", "red")), 
        main = "Differenced Data with Forecasts (%)",
        ylab="y-axis",
        xlab="Time")

forecast_og = predict(arima0, n.ahead = 12) # og
forecast_og
plot(forecast_og$pred, main = 
             "12 period ahead forecasts for the original data")
ts.plot(data_ts, predict(arima0, n.ahead = 12)$pred, gpars = list(col=c("black", "red")), 
        main = "Original Data with Forecasts (%)",
        ylab="y-axis",
        xlab="Time")

# accuracy

arima11 = arima(data_ts, order=c(0,1,1), seasonal=c(0,1,1))
arima22 = arima(data_ts, order=c(1,1,0), seasonal=c(0,1,1))
arima33 = arima(data_ts, order=c(1,1,0), seasonal=c(0,1,1))
arima44 = arima(data_ts, order=c(1,1,1), seasonal=c(0,1,1))
arima5 = arima(data_ts, order=c(1,1,2), seasonal=c(0,1,1))

accuracy(arima11)
accuracy(arima22)
accuracy(arima33)
accuracy(arima44)
accuracy(arima5)

# exporting to excel
export_f_og = data.frame(forecast_og)
library("writexl")
write_xlsx(export_f_og,"D:\\UofT\\Y2 Summer\\ECO374\\Essay\\test.xlsx")


