### Plotting the empirical autocorrelation and partial autocorrelation function 


acf(tseries, lag.max = 50)
plot(decompose(tseries))
pacf(tseries, 500)

acf(first_diff_tseries,50)
pacf(first_diff_tseries, 50)

# Function for testing the best fit with c(i,0,1) i = 1,...,3


arima(first_diff_tseries, c(3,0,1))
good <- arima(first_diff_tseries, c(1,0,1))
arima(first_diff_tseries, c(2,0,1))



plot(first_diff_tseries)
lines(first_diff_tseries-good$residuals, col = "red")

# Testing the independance of the residuals 

autoplot(good$residuals)

Box.test(good$residuals)

