setwd("D:/UofT/Y2 Summer/ECO374/HW4")

# Question 1. a.

data = read.csv("HWdata.csv")
expenditure_data = data.frame(data$LogExp)
y_ts = ts(expenditure_data,start=1962,frequency = 12)

t = seq(1:528)
t2 = t^2
t3 = t^3
t4 = t^4
t5 = t^5
t6 = t^6
t7 = t^7
t8 = t^8
t9 = t^9
t10 = t^10
t11 = t^11
t12 = t^12
t13 = t^13
t14 = t^14
m1 = lm(y_ts~t)
m2 = lm(y_ts~t+t2)
m3 = lm(y_ts~t+t2+t3)
m4 = lm(y_ts~t+t2+t3+t4)
m5 = lm(y_ts~t+t2+t3+t4+t5)
m6 = lm(y_ts~t+t2+t3+t4+t5+t6)
m7 = lm(y_ts~t+t2+t3+t4+t5+t6+t7)
m8 = lm(y_ts~t+t2+t3+t4+t5+t6+t7+t8)
m9 = lm(y_ts~t+t2+t3+t4+t5+t6+t7+t8+t9)
m10 = lm(y_ts~t+t2+t3+t4+t5+t6+t7+t8+t9+t10)
m11 = lm(y_ts~t+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11)
m12 = lm(y_ts~t+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12)
m13 = lm(y_ts~t+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13)
m14 = lm(y_ts~t+t2+t3+t4+t5+t6+t7+t8+t9+t10+t11+t12+t13+t14)
c(AIC(m1), AIC(m2),AIC(m3),AIC(m4),AIC(m5),AIC(m6),AIC(m7),AIC(m8),AIC(m9),AIC(m10),AIC(m11),AIC(m12),AIC(m13),AIC(m14))

y_fit = ts(m6$fitted.values,start=1962,frequency = 12)
ts.plot(y_fit, main = "Fitted Values of the Deterministic Trend", xlab = "Year", ylab = "Log Canadian Monthly Expenditure")
# ts.plot(y_fit, y_ts, gpars = list(col=c("blue", "red")))
y_res = ts(m6$residuals,start=1962,frequency = 12)


# Question 1. b.
library(aTSA)
Box.test(y_res, lag=1, type="Ljung-Box")
Box.test(y_res, lag=2, type="Ljung-Box")
Box.test(y_res, lag=3, type="Ljung-Box")
## reject the null hypothesis thus, there is autocorrelation

adf.test(y_res)
## therefore stationary, we only need ARMA not ARIMA

acf(y_res)
Acf(y_res)
pacf(y_res)
Pacf(y_res)

arima(y_res, order=c(2,0,0))
arima(y_res, order=c(2,0,1))
model = arima(y_res, order=c(2,0,1))
model

# Question 1. c.

N = 528
t = seq(N+1,N+24,1)
t2 = t^2 
t3 = t^3
t4 = t^4
t5 = t^5
t6 = t^6 
time = data.frame(t,t2,t3,t4,t5,t6)
m6_f = predict(m6,time)
m6_f = ts(m6_f,start=2006,frequency=12)
m6_f
ts.plot(ts(m6$fitted.values, start=1962, frequency=12), m6_f, 
        gpars = list(col = c("black", "red")), main = "24 periods ahead forecasts", 
        xlab = "Year", ylab = "Log Canadian Monthly Expenditure")


# Question 2.
library("tsDyn")

TB_file <- read.csv(file="TB3MS.csv")
tb=ts(TB_file$TB3MS, frequency = 12, start = c(1934,1))
threshold = 6
m = setar(tb, mL=1, mH=1, th=threshold)
summary(m)
tb_f = predict(m, n.ahead=48)
ts.plot(tb, tb_f, main="Forecast of 3-month US Treasury Bills Rate via SETAR 
        model", col=c("BLACK", "RED"))
legend(x="topleft", legend=c("3-month US Treasury Bills", "Forecast"), 
       col=c("BLACK", "RED"), lty=1, cex=1)
