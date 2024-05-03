# Loading the necessary packages

library(parsedate) # To manipulate the date
library(zoo) 
library(lubridate)
library(dygraphs) # Make interactive plot of the time series 
library(tidyverse) # Make data tidying easier

# Loading the series in a data frame

csvseries <- read_delim("data/series.csv", ";", col_names = c("Period", "IndexValue"), col_select = (1:2), skip = 4) 

head(csvseries, n = 5) 


# Formatting the table

csvseries <- mutate(csvseries, Period = as.yearmon(Period))
