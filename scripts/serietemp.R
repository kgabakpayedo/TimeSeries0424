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


#Q4
# Chargement des packages nécessaires
if (!require("forecast")) install.packages("forecast")
library(forecast)

# Analyse ACF et PACF
acf(diff_tseries, main = "Autocorrelation Function of differenced series")
pacf(diff_tseries, main = "Partial Autocorrelation Function of differenced series")
#on trouve p=6 q=7

# Définition des plages pour p et q d'apres les graphes precedents
p_max <- 6  # Changer selon PACF
q_max <- 7  # Changer selon ACF

# Initialisation des matrices pour stocker les résultats AIC et BIC
aic_matrix <- matrix(NA, nrow = p_max + 1, ncol = q_max + 1, dimnames = list(p = 0:p_max, q = 0:q_max))
bic_matrix <- matrix(NA, nrow = p_max + 1, ncol = q_max + 1, dimnames = list(p = 0:p_max, q = 0:q_max))

# Boucle pour ajuster les modèles ARMA et calculer AIC/BIC
for (p in 0:p_max) {
  for (q in 0:q_max) {
    model <- try(Arima(diff_tseries, order = c(p, 0, q)), silent = TRUE)
    if (!inherits(model, "try-error")) {
      aic_matrix[p + 1, q + 1] <- AIC(model)
      bic_matrix[p + 1, q + 1] <- BIC(model)
    }
  }
}

# Identifier les valeurs minimales de AIC et BIC
best_aic <- which(aic_matrix == min(aic_matrix, na.rm = TRUE), arr.ind = TRUE)
best_bic <- which(bic_matrix == min(bic_matrix, na.rm = TRUE), arr.ind = TRUE)

# Afficher les résultats
print(paste("Best ARMA(p,q) by AIC: (p =", best_aic[1] - 1, ", q =", best_aic[2] - 1, ")"))
print(paste("Best ARMA(p,q) by BIC: (p =", best_bic[1] - 1, ", q =", best_bic[2] - 1, ")"))

#on obtient ARMA(6,1) avec l'AIC et ARMA(1,1) avec le BIC 
#pour choisir 
# Modèle ARMA(6, 1) sélectionné par AIC
model_aic <- Arima(diff_tseries, order = c(6, 0, 1))
summary(model_aic)
checkresiduals(model_aic)

#test adequat aucune barre significative tous dans les zones

# Modèle ARMA(1, 1) sélectionné par BIC
model_bic <- Arima(diff_tseries, order = c(1, 0, 1))
summary(model_bic)
checkresiduals(model_bic) 

#test avec des barres significatives donc arma(6,1) mieux

# Comparaison des résidus
par(mfrow = c(2, 1))
acf(model_aic$residuals, main = "ACF of Residuals for ARMA(6,1)")
acf(model_bic$residuals, main = "ACF of Residuals for ARMA(1,1)")

#meme resultat 1
#De même, la p-value extrêmement basse rejette
#l'hypothèse de normalité des résidus. 
#Ce résultat est plus prononcé que pour le modèle ARMA(6,1), 
#indiquant potentiellement que le modèle ARMA(1,1) pourrait 
#être moins apte à modéliser les données 


# Test de Ljung-Box sur les résidus du modèle ARMA(6,1)
ljung_box_aic <- Box.test(model_aic$residuals, type = "Ljung-Box")
print("Résultats du test de Ljung-Box pour ARMA(6,1):")
print(ljung_box_aic)

# Test de Ljung-Box sur les résidus du modèle ARMA(1,1)
ljung_box_bic <- Box.test(model_bic$residuals, type = "Ljung-Box")
print("Résultats du test de Ljung-Box pour ARMA(1,1):")
print(ljung_box_bic)
# test plus adequat pour ARMA(6,1) pvalue plus grande



#Q5
#on choisit p=6 q=1






