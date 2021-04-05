getwd()

##############################################################
# This file loads the monthly retail sales per day.
# This data has been adjusted for inflation. 
# We will use the data to forecast the next two years of 
# sales per day.

# Created by : Ismail Khalil 
##############################################################


# clear all variables 

rm(list = ls())

# Load forcasting package

library(fpp2)

data <- read.csv('real_sales_per_day.csv')

# Declare this as time series data 

Y <- ts(data[,2], start = c(1992,1), frequency = 12)


##############################################################
# Preliminary Analysis 
##############################################################

# Time plot 

autoplot(Y)+
  ggtitle('Time Plot: Real US Retail Sales per Day')+
  ylab('Millions of 2017 dollars')

# Data has a trong trend. Investigate Transformations 

# Take the first difference of the data to remove the trend

DY <- diff(Y)

# Time plot of difference data 

autoplot(DY)+
  ggtitle('Time Plot: Change Real US Retail Sales per Day')+
  ylab('Millions of 2017 dollars')

# Series appears trend-stationary, need to investigate seasonality.

ggseasonplot(DY)+ 
  ggtitle('Seasonal Plot: Change in Daily Retail Sales')+
  ylab('Millions of 2017 dollars')


# Let's look at another seasonal plot, the subseries plot 

ggsubseriesplot(DY)


##############################################################
# Our serie Y, has trend and seasonality. 
# To remove the trend we take the first difference. 
# The first difference series still has seasonality. 
#
# Forcast with various methods.
##############################################################

#######
## Use a benchmark method to forcast 
# Let's use the seasonal naive method as our benchmark.
# y_t = y_{t-s} + e_t  // the value of january 1994 would be the same as in 1995
#######

fit <- snaive(DY)

summary(fit) # Residuals : 287.06

checkresiduals(fit)

#######
## fit ETS (Exponential Smoothing)
#######

fit_ETS <- ets(Y)
summary(fit_ETS) # Residuals : 218.8 // value fell, which means this model is performing better
checkresiduals(fit_ETS)


#######
## fit ARIMA model
# Data has to be stationary
#######

fit_arima <- auto.arima(Y, d = 1, D= 1, stepwise = FALSE, 
                        approximation = FALSE, trace = TRUE) # d = 1 means take the first difference of the data 
# D = 1 means getting rid of seasonality by taking the first seasonal difference

summary(fit_arima) # Residual SD :197.8105 
checkresiduals(fit_arima)


##############################################################
# Forecast with ARIMA model 
##############################################################

frct <- forecast(fit_arima, h= 24)
autoplot(frct, include = 60) # Last five years of data 

summary(frct)



