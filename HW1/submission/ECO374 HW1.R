setwd("D:/UofT/Y2 Summer/ECO374/HW1")

# Savings Rate
saving_rate = read.csv("PSAVERT.csv")
saving_rate = data.frame(saving_rate$PSAVERT)
colnames(saving_rate) = c("Saving Rate (%)")
saving_rate_ts = ts(saving_rate,start=1988,frequency = 12)
class(saving_rate_ts)
plot.ts(saving_rate_ts, 
        plot.type=c("single"), 
        col=c("blue"),
        main="Saving Rate (%), Monthly Data 1988/01-2008/02",
        ylab="%",
        xlab="Year")

# Home Price Index (12-Month Percentage Change)
HP_index = read.csv("SPCS10RSA.csv")
HP_index = data.frame(HP_index$SPCS10RSA_PC1)
colnames(HP_index) = c("S&P/Case-Shiller Home Price Indices")
HP_index_ts = ts(HP_index,start=1988,frequency = 12)
class(HP_index_ts)
plot.ts(HP_index_ts, 
        plot.type=c("single"), 
        col=c("blue"),
        main="S&P/Case-Shiller Home Price Indices",
        ylab="Percent change, year ago",
        xlab="Year")

# Exchange Rate US/Euro
exchange_rate_USEU = read.csv("DEXUSEU.csv")
exchange_rate_USEU = data.frame(exchange_rate_USEU$DEXUSEU)
colnames(exchange_rate_USEU) = c("Exchange Rate US/Euro")
exchange_rate_USEU_ts = ts(exchange_rate_USEU,start=1999,frequency = 12)
class(exchange_rate_USEU_ts)
plot.ts(exchange_rate_USEU_ts, 
        plot.type=c("single"), 
        col=c("blue"),
        main="Exchange Rate U.S.$/Euro, Monthly Data 1999/01-2008/03",
        ylab="U.S.$/Euro",
        xlab="Year")

# Exchange Rate Yen/US
exchange_rate_YENUS = read.csv("DEXJPUS.csv")
exchange_rate_YENUS = data.frame(exchange_rate_YENUS$DEXJPUS)
colnames(exchange_rate_YENUS) = c("Exchange Rate US/Euro")
exchange_rate_YENUS_ts = ts(exchange_rate_YENUS,start=1988,frequency = 12)
class(exchange_rate_YENUS_ts)
plot.ts(exchange_rate_YENUS_ts, 
        plot.type=c("single"), 
        col=c("blue"),
        main="Exchange Rate Japanese Yen/U.S. $, Monthly Data 1988/01-2008/03",
        ylab="Japanese Yen/U.S.$",
        xlab="Year")

# Treasury Rates
treasury_rates = read.csv("treasuryrates.csv")
treasury_rates = data.frame(cbind(treasury_rates$DTB3 ,treasury_rates$DGS10))
colnames(treasury_rates) = c("3-Month T-Bill", "10-year T-Bond")
treasury_rates_ts = ts(treasury_rates,start=1988,frequency = 12)
class(treasury_rates_ts)
plot.ts(treasury_rates_ts, 
        plot.type=c("single"), 
        col=c("blue", "red"),
        main="Treasury Rates: Short-Term Rate (3-Month T-Bill) and Long-Term 
        Rate (10-Year T-Bond), Monthly 1988/01-2008/03",
        ylab="Percent",
        xlab="Year")
legend(x="topright", legend=c("3-Month T-Bill", "10-year T-Bond"), 
                             col=c("blue", "red"), lty=1, cex=1)


# Q2

# GDP
GDP = read.csv("GDP.csv")
GDP_hist = hist(GDP$GDP_PCH)
summary(GDP)

# S&P500
SP500 = read.csv("SP500.csv")
SP500_hist = hist(SP500$SP500_PCH)
summary(SP500)


# Q3
library("dynlm")

GDP_growth = GDP$GDP_PCH
GDP_growth = data.frame(GDP_growth)
GDP_growth_ts = ts(GDP_growth,start=2012,frequency = 4)

SP500_growth = SP500$SP500_PCH
SP500_growth = data.frame(SP500_growth)
SP500_growth_ts = ts(SP500_growth,start=2012,frequency = 4)


# a.
reg1 = lm(GDP_growth_ts~SP500_growth_ts)
summary(reg1)


# b.
reg2 = dynlm(GDP_growth_ts~L(SP500_growth_ts, 1))
summary(reg2)


# c.
reg3 = dynlm(GDP_growth_ts~L(SP500_growth_ts, 1)+L(SP500_growth_ts, 2)+
                     L(SP500_growth_ts, 3)+L(SP500_growth_ts, 4))
summary(reg3)


# d.
reg4 = dynlm(GDP_growth_ts~L(SP500_growth_ts, 1)+L(SP500_growth_ts, 2)+
                     L(SP500_growth_ts, 3)+L(SP500_growth_ts, 4)+
                     L(GDP_growth_ts, 1))
summary(reg4)
