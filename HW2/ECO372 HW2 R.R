setwd("D:/UofT/Y2 Summer/ECO374/HW2")

# Chapter 4

file = read.csv("GBweb_Row_Format.csv")

# Exercise 6

file$dupe = as.integer(duplicated(file$DATE, fromLast = TRUE))
file = file[!file$dupe == 1,]

file$gRGDPB1 = as.numeric(file$gRGDPB1)

# ^^^ to clean the data

# TO DETERMINE THE FORECAST ERROR

realized_y = data.frame(file$gRGDPB1[2:196])
forecast_f = data.frame(file$gRGDPF0[1:195])
forecast_err = realized_y - forecast_f
colnames(forecast_err) = c("forecast_error")

# file$forecast_error = (file$gRGDPB1[2:196] - file$gRGDPF0[1:195])
# file_ts = ts(file[, 5:6], start=1967, frequency = 4)
# file_ts$forecast_error = (file_ts$gRGDPB1[2:196] - file_ts$gRGDPF0[1:195])

# Plotting data

# Realized Values
realized_values = data.frame(file$gRGDPB1)
colnames(realized_values) = c("realized_values (%)")
realized_values_ts = ts(realized_values,start=1967,frequency = 4)
plot.ts(realized_values_ts, 
        plot.type=c("single"), 
        col=c("blue"),
        main="Realized Growth in Real GDP (%)",
        ylab="%",
        xlab="Year")


# Forecasts
forecast_values = data.frame(file$gRGDPF0)
colnames(forecast_values) = c("forecast_values (%)")
forecast_values_ts = ts(forecast_values,start=1967,frequency = 4)
plot.ts(forecast_values_ts, 
        plot.type=c("single"), 
        col=c("blue"),
        main="Forecasted Growth in Real GDP (%)",
        ylab="%",
        xlab="Year")

# Forecast Error
forecast_error = forecast_err
colnames(forecast_error) = c("forecast_error (%)")
forecast_error_ts = ts(forecast_error,start=1967,frequency = 4)
plot.ts(forecast_error_ts, 
        plot.type=c("single"), 
        col=c("blue"),
        main="Forecast Error in the Growth in Real GDP (%)",
        ylab="%",
        xlab="Year")

# Descriptive Statistics
summary(realized_values)
hist(file$gRGDPB1)
sd(file$gRGDPB1)

summary(forecast_values)
hist(file$gRGDPF0)
sd(file$gRGDPF0)

summary(forecast_error)
hist(forecast_err$forecast_error)
t.test(forecast_error)

# ACF
# // Realized
acf(realized_values_ts)
acf(realized_values_ts, plot=FALSE)
pacf(realized_values_ts)
pacf(realized_values_ts, plot=FALSE)

# // Forecast
acf(forecast_values_ts)
acf(forecast_values_ts, plot=FALSE)
pacf(forecast_values_ts)
pacf(forecast_values_ts, plot=FALSE)

# // Forecast Error
acf(forecast_error_ts)
acf(forecast_error_ts, plot=FALSE)
pacf(forecast_error_ts)
pacf(forecast_error_ts, plot=FALSE)

# Question 7

reg1 = lm(realized_values_ts~forecast_values_ts)

# // plot
plot.ts(realized_values_ts, forecast_values_ts, 
        plot.type=c("single"), 
        col=c("blue"),
        main="Realized Values against the Forecasts in Real GDP",
        ylab="Realized Growth in Real GDP (%)",
        xlab="Forecasted Growth in Real GDP (%)")
abline(lm(realized_values_ts~forecast_values_ts))

# // regression
summary(reg1)


# Question 8
library("dynlm")

t.test(forecast_error_ts)

reg2 = dynlm(forecast_error_ts~L(forecast_error_ts, 1)+L(forecast_error_ts, 2)+
               L(forecast_error_ts, 3))
summary(reg2)

# Question 9

reg3 = dynlm(forecast_error_ts~L(forecast_error_ts, 1)+L(forecast_error_ts, 2)+
               L(forecast_values_ts, 0)+L(forecast_values_ts, 1))
summary(reg3)

library("car")
H_0 = c("L(forecast_values_ts, 0) = 0", "L(forecast_values_ts, 1) = 0")
linearHypothesis(reg3, H_0)


# Chapter 6

# Exercise 4(b)
library("car")
set.seed(1005815806) # used my student number
n = 100
eps = rnorm(n)

y = 0.7 + arima.sim(list(ma=c(-2,1.35)), 100, innov = eps)
acf(y,plot=FALSE)
acf(y)

