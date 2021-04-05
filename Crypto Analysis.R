# install.packages("coinmarketcapr")
getwd()
library(coinmarketcapr)
library(tidyverse)

my_key <- '4a200f88-23a0-4883-82ad-92bcdfea004e'

coinmarketcapr::setup(my_key)

cryptos <- get_crypto_listings(limit = 60)

marketcap <- get_global_marketcap()

t(marketcap)

cryptos %>%
  head(10)  %>%
  select(c('name', 'USD_price', 'USD_percent_change_7d')) -> top_10

################### EMAIL THEM plot_top_currencies(currency = "USD")
top_10  %>%
  ggplot() + geom_col(aes(x = name,
                          y = USD_price))

########## Check the crypto with the most changes during the last 7 days 
sorted_USD_percent_change_7d <- cryptos[order(USD_percent_change_7d)]

head(sorted_USD_percent_change_7d$name) 

# sort by USD_price_change & price in the last 30 days and price 
sorted_USD_percent_change_30d <- cryptos[order(USD_percent_change_30d,USD_price),2]
head(sorted_USD_percent_change_30d,20) 




######################################################### Focus on Etherium 

library(prophet)
library(lubridate)
library(ggplot2)

Ethereum <- read.csv("Ethereum.csv", header = T)

Ethereum$Date <- dmy(Ethereum$Date)

str(Ethereum)
head(Ethereum)

qplot(Date, Close, data = Ethereum, main = "Etherium Closing Price 2015 - 2020")

# In order to see the patterns when the values are low or high we can do a log transformation 
# Log transformation
ds <- Ethereum$Date
y <- log(Ethereum $Close)
df <- data.frame(ds, y)


qplot(ds, y, data = df, main = "Etherium Closing Price in log scale 2015 - 2020")
# W are now Able to see patterns when values are very low 



# Forecasting with Facebook's prophet package
m <- prophet(df)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

# Plot forecast
plot(m, forecast)
prophet_plot_components(m, forecast)
dyplot.prophet(m,forecast)
#for the price just take the exp 
#Aug 13 2017 Actual: 5.7 , prediccted 5.64 
exp(5.64)
exp(5.7)
# price predicted 281.4627 , Actual price 298.86



# Model performance
pred <- forecast$yhat[1:1544]
actual <- m$history$y
plot(actual, pred)
abline(lm(pred~actual))
summary(lm(pred~actual))

x <- cross_validation(m, 365, units = 'days')
performance_metrics(x, rolling_window = 0.1)
plot_cross_validation_metric(x,
                             metric = 'rmse',
                             rolling_window = 0.1)
