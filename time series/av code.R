
install.packages("forecast")
install.packages("tseries")

library(forecast)
library(tseries)
data(AirPassengers)


#This tells you that the data series is in a time series format
class(AirPassengers)

#This is the start and end of the time series
start(AirPassengers)
end(AirPassengers)

#The cycle of this time series is 12months in a year
frequency(AirPassengers)

#The number of passengers are distributed across the spectrum
summary(AirPassengers)

#This will plot the time series
plot(AirPassengers)

# This will fit in a line
abline(reg=lm(AirPassengers~time(AirPassengers)))

#This will print the cycle across years.
cycle(AirPassengers)

#This will aggregate the cycles and display a year on year trend
plot(aggregate(AirPassengers,FUN=mean))

#Box plot across months will give us a sense on seasonal effect
boxplot(AirPassengers~cycle(AirPassengers))

#Augmented Dickey-Fuller Test
adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)

#ACF Plots
acf(log(AirPassengers))

#Clearly, the decay of ACF chart is very slow, which means that the population is not stationary. 
#We have already discussed above that we now intend to regress onthe difference of logs rather 
#than log directly. Let’s see how ACF and PACF curve come out after regressing on the difference.
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))

#Clearly, ACF plot cuts off after the first lag. Hence, we understood that value of p should be 0 
#as the ACF is the curve getting a cut off. While value of q should be 1 or 2. After a few iterations, 
#we found that (0,1,1) as (p,d,q) comes out to be the combination with least AIC and BIC.




#Let’s fit an ARIMA model and predict the future 10 years. Also, we will try fitting in a seasonal 
#component in the ARIMA formulation. Then, we will visualize the prediction along with the training data. 
#You can use the following code to do the same :

(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))



