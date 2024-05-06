# Loading the packages to run statistical operations on the series

library(tseries)
library(forecast)



# Function to check stationnarity by performing ADF and KPSS test and plotting the time series 

# Considering the first-order and second-order differences



first_diff_tseries <- na.omit(diff(log(tseries)))

dygraph(first_diff_tseries)

print(adf.test(first_diff_tseries))
print(kpss.test(first_diff_tseries))


two_diff_tseries <- na.omit(diff(tseries, differences = 2))

dygraph(two_diff_tseries)

print(adf.test(two_diff_tseries))
print(kpss.test(two_diff_series))


# Considering the seasonal difference of the 12-th lag

seasonal_diff_tseries <- na.omit(diff(tseries, lag = 12))

dygraph(seasonal_diff_tseries)

print(adf.test(seasonal_diff_tseries))
print(kpss.test(seasonal_diff_tseries))