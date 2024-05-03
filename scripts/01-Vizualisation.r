# Loading the necessary packages

library(parsedate) # To manipulate the date
library(zoo) 
library(lubridate)
library(dygraphs) # Make interactive plot of the time series 
library(tidyverse) # Make data tidying easier

# Loading the series in a data frame

series <- read_delim("data/series.csv", ";", col_names = c("Period", "IndexValue"), col_select = (1:2), skip = 4) 

head(series, n = 5) 


# Formatting the table

series <- mutate(series, Period = as.yearmon(Period))

glimpse(series)

# Creating a time series object from the original series 

tseries <- xts(series$IndexValues, order.by = series$IndexValues)

dygraph(tseries)

