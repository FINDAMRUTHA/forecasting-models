

library(tseries)
#read the data set
 cereal_orgnl<-read.csv("cereal.csv")
#make the corresponding table
cereal_cleaned_1<-head(cereal_orgnl,9)
cereal_cleaned_2<-tail(cereal_cleaned_1, 2)
cereal_cleaned_3<-cereal_cleaned_2[, -1]
library(data.table)
cereal_data<-transpose(as.data.table(cereal_cleaned_2))
cereal_main<-cereal_data[-1,]
colnames(cereal_main)<-c("YEAR","PRODUCTION")
#to plot the original data
t<-plot(cereal_main$YEAR,cereal_main$PRODUCTION, xlab="Year"
        ,ylab="Cereal production", main = " plot of cereal 
        production over years",col="black")
lines(cereal_main$YEAR,cereal_main$PRODUCTION,col="red")
print(t)
#to plot the trend
linear_model <- lm(PRODUCTION ~ YEAR, data =cereal_main)
# Extract the coefficients
intercept <- coef(linear_model)[1]
slope <- coef(linear_model)[2]

trend_values <- predict(linear_model)
cat("Intercept:", intercept, "\n")
cat("Slope (Trend):", slope, "\n")
trend_line1 <- function(x) {
    return(intercept1 + (slope1 * x))
   }
plot(trend_line1,main="TREND LINE OF CEREAL PRODUCTION",
     col ="blue")
legend("bottomright", legend = paste("Trend Line: y =",
  round(intercept, 2), "+", round(slope, 2), "x"), 
  col = "blue", lty = 1)

#plot acf and pacf
cereal_ts <- cereal_main$PRODUCTION
cereal_ts1 <- ts(cereal_ts, start = min(cereal_main$YEAR), 
                 end = max(cereal_main$YEAR), frequency = 1)
class(cereal_ts1)
cereal_ts1<-as.numeric(cereal_main$PRODUCTION)
acf(cereal_ts1, main = "Autocorrelation Function (ACF)")
pacf(cereal_ts1, main = "Partial Autocorrelation Function (PACF)")
# adf test
install.packages("tseries")
library(tseries)
adf.test(cereal_ts1,alternative = "stationary")


# auto arima
install.packages("forecast")
library(forecast)
model0 <- arima(cereal_ts1, order = c(0, 0, 0))
summary(model0)
n.ahead <- 10  # Number of steps ahead for which prediction is required
pred_10 <- predict(model0, n.ahead = n.ahead)$pred
print(pred_10)
plot(pred_10)
# AR MODEL
model2 <- arima(cereal_ts1, order = c(1, 0, 0))
summary(model2)
n.ahead <- 10  # Number of steps ahead for which prediction is required
pred_12 <- predict(model2, n.ahead = n.ahead)$pred
print(pred_12)
fy_2 <- seq(2023, 2032)
plot(fy_2, pred_12, type = 'l', xlab = 'Year', ylab = 'Future Production', main = 'Future Production Forecast by AR(1) model')

plot(pred_12)
#MA MODEL
model3 <- arima(cereal_ts1, order = c(0,0,1))
summary(model3)
n.ahead <- 10  # Number of steps ahead for which prediction is required
pred_13<- predict(model3,n.ahead = n.ahead)$pred
print(pred_13)
fy_3 <- seq(2023, 2032)
plot(fy_3, pred_13, type = 'l', xlab = 'Year', ylab = 'Future Production', main = 'Future Production Forecast by MA(1) model')

# Fit a ARMA model of order (1, 1)
model4<- arima(cereal_ts1, order = c(1, 0, 1))
summary(model4)
n.ahead <- 10  # Number of steps ahead for which prediction is required
pred_14<- predict(model4,n.ahead = n.ahead)$pred
print(pred_14)

fy_4 <- seq(2023, 2032)
plot(fy_4, pred_14, type = 'l', xlab = 'Year', ylab = 'Future Production', main = 'Future Production Forecast by ARMA(1,1)')







