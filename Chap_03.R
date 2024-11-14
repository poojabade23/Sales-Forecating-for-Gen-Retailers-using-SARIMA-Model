#Read the data from the working directory, create your own working directly to read the dataset.

setwd("C:\\Users\\Dell\\Desktop\\fall 2024\\bua 650")

data1 <- read.csv ("Chapter_03.csv",header=TRUE,sep=",")


#Convert data into time series data

data <- ts(data1[,2],start = c(1992,1),frequency = 12)

data


#Perform exploratory data analysis to know about the time series data

#Displays the start date of the time series data

start(data)




# displays the end date of the time series data


end(data)




#Displays the frequency of the time series data whether   monthly, quaterly, weekly.


frequency(data)




#Displays the data type it is time series data

class(data)



#Displays descriptive statistics of the time series data

summary(data)




#Checking the missing values present in the time series data

is.na(data)


[1]  FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
[11] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
[21] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE


#Plotting the time series data

plot(data, xlab='Years', ylab = 'Sales')



#Install astsa package

install.packages("astsa")

library(astsa)

#To see acf and pacf in original data

acf2(data,max.lag = 24)


#Seasonally differenced retail sales

datadiff12 <- diff(data,12)

#Plot seasonally differenced retail sales

plot.ts(datadiff12)


#Trend and seasonally differenced retail sales

diff1and12=diff(datadiff12,1)

#Plot Trend and seasonally differenced retail sales

plot(diff1and12)



#To see acf and pacf of trend and seasonally differenced retail sales

acf2(diff1and12,max.lag = 36)



#Install forecast package

install.packages("forecast")

library(forecast)


#Building seasonal 〖ARIMA(2,1,1)(2,1,2)〗_12    model

model1<- arima(data,order=c(2,1,1),seasonal=list
               (order=c(2,1,2),period=12))

summary(model1)


# Portmanteau or Box-Ljung test to check whether residuals are white noise

Acf(residuals(model1))

Box.test(residuals(model1),lag=24,fitdf =1,type="Ljung")


#Rebuilding 〖ARIMA(6,1,1)(2,1,2)〗_12 model with different non seasonal terms


model2 <- arima(data,order=c(6,1,1),seasonal= list
                (order=c(2,1,2),period=12))


summary(model2)


# Portmanteau and Box-Ljung test on model2 to check whether residual are white noise

Acf(residuals(model2))


Box.test(residuals(model2),lag=24,fitdf = 1,type="Ljung")


#Forecast for the next 30 month

Pred <- forecast(model2,h=30)
Pred

# Install the tseries package if you haven't already
install.packages("tseries")
library(tseries)

# Apply the ADF test on your time series data
# 'data' is assumed to be your time series object
adf_test <- adf.test(data, alternative = "stationary")

# Print the test results
print(adf_test)

#Creating the plot for forecast retail sales

plot(Pred,ylab="sales (million in dollars)",xlab="Year")


library(tseries)
adf_result<- adf.test(data)
print(adf_result)

library(forecast)
best_model<- auto.arima(data)
summary(best_model)

# Install and load the forecast package if you haven't already
install.packages("forecast")
library(forecast)

# Apply the auto.arima function to find the best model
auto_model <- auto.arima(data, seasonal = TRUE)

# Print the model summary
summary(auto_model)

# Forecast the next 30 periods (months in this case)
forecast_auto <- forecast(auto_model, h = 30)

# Plot the forecast
plot(forecast_auto, main = "30-Month Forecast Using Auto ARIMA", ylab = "Sales", xlab = "Time")

