setwd("D:/UofT/Y2 Summer/ECO374/HW3")

# # Q1 Part a.
CAPCPI = read.csv("CAPCPI.csv")
CAPCPI = data.frame(CAPCPI$CAPCPI_PCH)
# colnames(CAPCPI) = c("Per Capita Income Growth in California (%)")
CAPCPI_ts = ts(CAPCPI,start=1970,frequency = 1)
class(CAPCPI_ts)
plot.ts(CAPCPI_ts, 
        plot.type=c("single"), 
        col=c("blue"),
        main="Per Capita Income Growth in California (%), Annual Data 1970/01-
        2020/01",
        ylab="% Change",
        xlab="Year")

# # Q2 Part b.
acf(CAPCPI_ts, main = "CAPCPI")
acf(CAPCPI_ts, plot=FALSE)

pacf(CAPCPI_ts, main = "CAPCPI")
pacf(CAPCPI_ts, plot=FALSE)

# not stationary thus, we first difference
Yt = data.frame(CAPCPI$CAPCPI.CAPCPI_PCH[2:51])
Yt_1 = data.frame(CAPCPI$CAPCPI.CAPCPI_PCH[1:50])
Zt = Yt - Yt_1
CAPCPI_FD_ts = ts(Zt,start=1970,frequency = 1)

# calculating ACF again
acf(CAPCPI_FD_ts, main = "CAPCPI FD")
acf(CAPCPI_FD_ts, plot=FALSE)

# ACF is stationary, calculate PACF
pacf(CAPCPI_FD_ts, main = "CAPCPI FD")
pacf(CAPCPI_FD_ts, plot=FALSE)

# # Q2 part d.
# model estimation
m1 = arima(CAPCPI_ts, order=c(1,1,1))
m1

# forecasts
f3 = predict(m1, n.ahead = 3)
f3
plot(f3$pred, main = "1-step-, 2-step-, and 3-step- ahead forecasts")
ts.plot(CAPCPI_ts, f3$pred, gpars = list(col=c("black", "red")), 
        main = "Per Capita Income Growth in California with Forecasts (%)",
        ylab="% Change",
        xlab="Year")

library(forecast)
auto.arima(CAPCPI_ts)
auto.arima(CAPCPI_FD_ts)