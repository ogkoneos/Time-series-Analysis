library(astsa) 
library(MASS)
library(forecast)
consumers <- read.csv('total-number-of-water-consumers-.csv', header=FALSE)
consumers

#Organizing data
datafull <- ts(consumers[2], frequency = 12, start = c(1983,1))
data <- ts(consumers[2], frequency = 12, start = c(1983,1), end = c(1993,6))
data
plot(datafull)
data[66] <- (data[65]+data[67])/2
data[66]

#Transformations
plot(data, main='Total London Water Consumers', ylab='Consumers')
acf(data, main='ACF of Original data')
pacf(data, main='PACF of Original data')
dataseasonal <- diff(diff(log(data),12))
plot(dataseasonal)
acf(dataseasonal, main='ACF of Transformed data')
pacf(dataseasonal, main='PACF of Transformed data')



#Seasonality, trend, residuals
bt <- stl(data, 'periodic')
plot(bt)
at <- stl(dataseasonal, 'periodic')
plot(at)
par(mfrow=c(1,1))

#model
model1 <-sarima(data,2,1,2,1,0,1,12)
model2 <-sarima(data,2,1,2,1,0,0,12)
model3 <-sarima(data,2,1,2,0,0,1,12)
model1
model2
model3

c(model1$BIC,model2$BIC,model3$BIC)
c(model1$AIC,model2$AIC,model3$AIC)
model3$fit$sigma2

consumer.for <- sarima.for(data,10,2,1,2,0,0,1,12)
consumer.for$pred

#forecasting previous values
dataprecast <- ts(data[1:116], frequency = 12, start = c(1983,1))
dataprecast
consumerback.for <- sarima.for(dataprecast,10,2,1,2,0,0,1,12)
consumerback.for$pred
consumerback.for$se


#Full graph of precast, and forecast
plot(ts(dataprecast[97:116], frequency=12, end=c(1992,8)), xlim=c(1992.8,1994.5), ylim=c(25000,40000), main='Predicted and Actual', ylab='Consumers')
points(consumerback.for$pred, col='blue', type='b')
points(consumerback.for$pred+1.96*consumerback.for$se, type='l', col='green', lty='dashed')
points(consumerback.for$pred-1.96*consumerback.for$se, type='l', col='green', lty='dashed')
points(consumer.for$pred, col='red', type='b')
points(consumer.for$pred+1.96*consumer.for$se, type='l', col='purple', lty='dashed')
points(consumer.for$pred-1.96*consumer.for$se, type='l', col='purple', lty='dashed')
points(datafull, type='l', col='orange')
points(data, type='l', col='black')

