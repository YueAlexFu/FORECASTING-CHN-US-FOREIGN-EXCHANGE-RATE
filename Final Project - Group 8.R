# DATA EXPLORATION 
setwd("C:/Users/even4/OneDrive/Desktop/Fin642 Bus Conditions & Forecast/group project")
library(fpp2)
DEXCHUS <- read.csv("DEXCHUS.csv")
DEXCHUS <- ts(DEXCHUS$DEXCHUS, start=2014, frequency=12) 
ggtsdisplay(DEXCHUS)

BoxCox.lambda(DEXCHUS)

fit <- stl(DEXCHUS, s.window = "periodic") # Decomposition
max(0,1-var(remainder(fit))/var(remainder(fit)+trendcycle(fit))) # FT
max(0,1-var(remainder(fit))/var(remainder(fit)+seasonal(fit))) # FS

train <- window(DEXCHUS, end = c(2018,12)) # make training data for DEXCHUS
test <- window(DEXCHUS, start = c(2019,1)) # make test data for DEXCHUS

h <- length(test)
h # number of months to forecast

# FORECASTING MODELS
# Simple forecasts
fc0 <- meanf(train, h = h) # Average method
fc1 <- snaive(train, h = h) # Seasonal naive method
fc2 <- naive(train, h = h) # Naive method
fc3 <- rwf(train, h = h, drift=TRUE) # Drift method
fc4 <- forecast(tslm(train ~ trend + season), h = h) # Linear regression
fc5 <- forecast(tslm(train ~ trend), h = h) # Linear regression 

autoplot(DEXCHUS) +
  ggtitle("Monthly average CNY/USD since 2014") +
  ylab("Rate") +
  autolayer(fc0, series="Mean", PI = FALSE) +
  autolayer(fc1, series="Snaive", PI = FALSE) +
  autolayer(fc2, series="Naive", PI = FALSE) +
  autolayer(fc3, series="Drift", PI = FALSE) +
  autolayer(fc4, series="Linear T+S", PI = FALSE) +
  autolayer(fc5, series="Linear T", PI = FALSE)

fc0.acc <- accuracy(fc0,test)[2,]
fc1.acc <- accuracy(fc1,test)[2,]
fc2.acc <- accuracy(fc2,test)[2,]
fc3.acc <- accuracy(fc3,test)[2,]
fc4.acc <- accuracy(fc4,test)[2,]
fc5.acc <- accuracy(fc5,test)[2,]

table <- rbind(fc0.acc,fc1.acc,fc2.acc,fc3.acc,fc4.acc,fc5.acc) 
rownames(table) <- c("Mean","Snaive","Naive","Drift","Linear T+S","Linear T")
table[,c(2,3,5,6)]

# Exponential smoothing
ets(train) 
fc6 <- stlf(train, h = h, etsmodel = "AAN", damped = T) # Damped AAN
fc7 <- stlf(train, h = h, etsmodel = "AAN", damped = F) # AAN
fc8 <- holt(train, h = h) # Holt's linear trend
fc9 <- forecast(ets(train), h = h) # ETS

autoplot(DEXCHUS) +
  ggtitle("Monthly average CNY/USD since 2014") +
  ylab("Rate") +
  autolayer(fc6, series="Damped AAN", PI = FALSE) +
  autolayer(fc7, series="AAN", PI = FALSE) +
  autolayer(fc8, series="Holt", PI = FALSE) +
  autolayer(fc9, series="ETS", PI = FALSE)

fc6.acc <- accuracy(fc6,test)[2,]
fc7.acc <- accuracy(fc7,test)[2,]
fc8.acc <- accuracy(fc8,test)[2,]
fc9.acc <- accuracy(fc9,test)[2,]

table <- rbind(fc6.acc,fc7.acc,fc8.acc,fc9.acc) 
rownames(table) <- c("Damped AAN","AAN","Holt","ETS")
table[,c(2,3,5,6)]

# ARIMA
fc10 <- forecast(auto.arima(train), h = h)

autoplot(DEXCHUS) +
  ggtitle("Monthly average CNY/USD since 2014") +
  ylab("Rate") +
  autolayer(fc10, series="ARIMA", PI = FALSE)

accuracy(fc10,test)[2,c(2,3,5,6)]

# Residuals
checkresiduals(fc4)
checkresiduals(fc6)
checkresiduals(fc10)
