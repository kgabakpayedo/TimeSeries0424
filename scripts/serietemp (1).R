# Loading the necessary packages
install.packages("parsedate")
install.packages("zoo")
install.packages("lubricate")
install.packages("dygraphs")
install.packages("tidyverse")
install.packages("FitAR")
install.packages("urca")
library(parsedate) # To manipulate the date
library(zoo) 
library(lubridate)
library(dygraphs) # Make interactive plot of the time series 
library(tidyverse) # Make data tidying easier
library(urca)
#q1
# Loading the series in a data frame

series <- read_delim("/Users/youssef/Desktop/series.csv", ";", col_names = c("Period", "IndexValue"), col_select = (1:2), skip = 4)

head(series, n = 5) 


# Formatting the table

series <- mutate(series, Period = as.yearmon(Period))

glimpse(series)

# Creating a time series object from the original series 

tseries <- ts(series$IndexValue, frequency = 12, start = c(1998,1))
dygraph(tseries)


#2
# Loading the packages to run statistical operations on the series
install.packages("tseries")
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
# Installation et chargement des packages nécessaires
install.packages("tseries")
install.packages("urca")
library(tseries)
library(urca)

# Chargement des données

# Création d'un objet série temporelle avec fréquence mensuelle
tseries <- ts(series$IndexValue, frequency = 12, start = c(1998,1))

# Test de Dickey-Fuller augmenté
adf_result <- adf.test(tseries)
print(adf_result)
#serie non stationnaire pvalue>0.5

# Test Phillips-Perron
pp_result <- ur.pp(tseries, type = "Z-tau")
print(pp_result@teststat)
print(pp_result@cval)
# tstat>valeur usuel serie non stationnaire

# Test KPSS pour tester la stationnarité
kpss_result <- ur.kpss(tseries)
print(kpss_result@teststat)
print(kpss_result@cval)
# t stat> valeur usuel serie non stationnaire 

# Différenciation de la série si nécessaire
# Ici,comme la serie est non stationnaire on fait le test sur la serie differencier 
diff_tseries <- diff(tseries)

# Test ADF sur la série différenciée
adf_diff_result <- adf.test(diff_tseries)
print(adf_diff_result)
#pval=0.01 serie stat 

# Test Phillips-Perron sur la série différenciée
pp_diff_result <- ur.pp(diff_tseries, type = "Z-tau")
print(pp_diff_result@teststat)
print(pp_diff_result@cval)

#t stat tres faible serie stat 

# Test KPSS sur la série différenciée
kpss_diff_result <- ur.kpss(diff_tseries)
print(kpss_diff_result@teststat)
print(kpss_diff_result@cval)
 # tstat faible serie stat 


#Q3
library(tidyverse)
library(lubridate)
library(dygraphs)

# Chargement des données
series <- read_delim("/Users/youssef/Desktop/series.csv", ";", col_names = c("Period", "IndexValue"), col_select = (1:2), skip = 4)


# Création d'un objet série temporelle
tseries <- ts(series$IndexValue, frequency = 12, start = c(1998,1))

# Première différenciation
diff_tseries <- diff(tseries)

# Tracé des graphiques
# Pour la série originale
original_plot <- dygraph(tseries) %>%
  dyOptions(drawPoints = TRUE, pointSize = 2) %>%
  dyAxis("y", label = "Valeur de l’Indice") %>%
  dyAxis("x", label = "Période")

# Pour la série différenciée
diff_plot <- dygraph(diff_tseries) %>%
  dyOptions(drawPoints = TRUE, pointSize = 2, colors = RColorBrewer::brewer.pal(1, "Set1")) %>%
  dyAxis("y", label = "Valeur de l’Indice") %>%
  dyAxis("x", label = "Période")

# Affichage des graphiques
print(original_plot)
print(diff_plot)

