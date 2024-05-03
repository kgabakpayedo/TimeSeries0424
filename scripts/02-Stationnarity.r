# Loading the packages to run statistical operations on the series

library(tseries)


# Considering the first-order and second-order differences

diff_tseries <- na.omit(diff(tseries))

two_diff_tseries <- na.omit(diff(diff_tseries))


# Making a seasonal trend and residuals decomposition and printing the 3 components together

dec_tseries <- decompose(tseries)

trend <- dec_tseries$trend
seasonal <- dec_tseries$seasonal
random <- dec_tseries$random

 

trend_graph <- dygraph(trend)
seasonal_graph <- dygraph(seasonal)
random_graph <- dygraph(random)


print(trend_graph)
print(seasonal_graph)
print(random_graph)

nf <- layout(matrix(c(1:3),3,1, byrow = TRUE))
layout.show(nf)




# Performing the ADF test to check stationnarity

adf_result <- adf.test(tseries)

print(adf_result)
