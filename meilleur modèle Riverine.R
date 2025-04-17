library(readxl)
library(zoo)
library(ggplot2)
library(dplyr)
library(tidyr)
library(EnvStats)
library(moments)
library(tibble)
library(seasonal)
library(RJDemetra)
library(forecast)
library(tsoutliers)
library(smooth)
library(gridExtra)

# Statistiques descriptives ----

stats_desc <- function(data) {
  # Calcul des statistiques de base
  moyenne <- mean(data, na.rm = TRUE)
  ecart_type <- sd(data, na.rm = TRUE)
  asymetrie <- skewness(data, na.rm = TRUE)
  aplatissement <- kurtosis(data, na.rm = TRUE)
  shapiro_test <- shapiro.test(data)$p.value  # Test de normalité (Shapiro-Wilk)
  
  # Calcul des quantiles
  quantiles <- quantile(data, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  
  # Création d'un tableau de résultats
  resultats <- tibble(
    Statistique = c("Moyenne", "Écart-type", "Skewness", "Kurtosis", 
                    "p-value Shapiro-Wilk", "Min", "1er Quartile (Q1)", 
                    "Médiane (Q2)", "3e Quartile (Q3)", "Max"),
    Valeur = c(moyenne, ecart_type, asymetrie, aplatissement, shapiro_test, 
               quantiles[1], quantiles[2], quantiles[3], quantiles[4], quantiles[5])
  )
  
  return(resultats)
}


ts_riverine_flood_corr |> 
  stats_desc()

ts_riverine_flood_corr |> 
  boxplot()

# Saisonnalité ? ----

ts_riverine_flood_corr |> 
  decompose(, type = "additive") |> 
  plot()

ts_riverine_flood_corr |> 
  decompose(, type = "multiplicative") |> 
  plot()


# Test saisonnalité

# summary(regarima_x13(ts_riverine_flood_corr, spec ="RG5c"))


library(TSA)
dyy <- diff(ts_riverine_flood_corr, differences = 1)

# Periodogramme
par(mfrow=c(1,2))
periodogram(ts_riverine_flood_corr, main="Periodogramme sur la série en niveau")
periodogram(dyy, main="Periodogramme sur la série en différence première")


## dire des choses ici
## ça à l'air Additif

# Estimer et prévoir ----

#-- 5. Prévision de la série saisonnière corrigée des points atypiques sur une année ----------

#-- 5. 1. Estimer et prévoir les modèles suivants ----------

#-- 5. 1. 1. Les méthodes naïves ----------

####  StructTS ----

fitsts = StructTS(ts_riverine_flood_corr)
plot(cbind(fitted(fitsts), residuals(fitsts)))
show(fitsts)

summary(fitsts)
prevsts <- forecast(fitsts, 12) 
show(prevsts) 
plot(prevsts)

#### stlm ----

decomp = stl(ts_riverine_flood_corr, s.window="periodic")
# show(decomp)
plot(decomp)

fitstl = stlm(ts_riverine_flood_corr)

prevstl <- forecast(fitstl,12) #période d'une année

# show(prevsts) # pas mettre en annexe

plot(prevstl) # en annexe

summary(prevstl)

#### X13 ----

myspec <- x13_spec("RSA5c")

mysax13 <- x13(ts_riverine_flood_corr, myspec)

summary(mysax13$regarima)

mysax13

plot(mysax13$final)

# Extraire la série désaisonnalisée
sa_series <- mysax13$final$series[, "sa"]

# Appliquer la prévision sur 12 périodes
forecast_x13 <- forecast(sa_series, h = 12)

# Visualiser le résultat
plot(forecast_x13)


#-- 5. 1. 2. Prédiction sur les méthodes de lissage exponentiel ----------


#### Holt-winters ----

seasX <- seas(ts_riverine_flood_corr)
cvs <- final(seasX)

# lissage
WH_add = HoltWinters(cvs) # partie constante, tendance et saisonnalité et choix additif ou multiplicatif
#WH_add = HoltWinters(cvs, seasonal = "mul") # il suppose un modèle add de base donc je change à la main
show(WH_add)
summary(WH_add)
plot(WH_add)
plot(WH_add$fitted[,1])

# Calcul de l'AIC
# Calcul des résidus
residuals <- residuals(WH_add)

# Calcul du MSE
mse <- mean(residuals^2)

# Estimation de la vraisemblance
n <- length(cvs)  # nombre d'observations
k <- length(coef(WH_add))  # nombre de paramètres
log_likelihood <- -(n/2) * log(2 * pi * mse) - (1/2) * sum(residuals^2 / mse)


aic_value <- 2 * k - 2 * log_likelihood
print(aic_value)

# Calcul de l'AICc
aicc_value <- aic_value + (2 * k * (k + 1)) / (n - k - 1)
print(aicc_value)

# horizon h=50 - intervals 80% & 95%
library(forecast)
fit = forecast(WH_add, h=50)
plot(fit)
show(fit)

#### ETS ----

fitets <- ets(ts_riverine_flood_corr, ic = "aic") # pour avoir le meilleur AIC
show(fitets)
plot(fitets)


prevets <- forecast(fitets,12)
show(prevets)
plot(prevets)

#### TBATS ----

fit_tbats <- tbats(ts_riverine_flood_corr)
show(fit_tbats)
plot(fit_tbats)

prev_TBATS <- forecast(fit_tbats, h=12)
plot_prev_TBATS <- plot(prev_TBATS)

#### ADAM ETS ----

fit_ADAM_ETS <- auto.adam(ts_riverine_flood_corr, model = "ZZZ", lags = c(1, 12), select = TRUE)
# ZZZ car je ne spécifie rien (tendance, saisonnalité, erreur)
fit_ADAM_ETS
summary(fit_ADAM_ETS)

par(mfcol=c(2,2))
plot(fit_ADAM_ETS)

par(mfcol=c(1,1))


plot(fit_ADAM_ETS$states)
plot(fit_ADAM_ETS$residuals)


prev_ADAM_ETS <- forecast(fit_ADAM_ETS, h=12)
show(prev_ADAM_ETS)
plot(prev_ADAM_ETS)

#### ADAM ETS + SARIMA ----

fitadam3 <- auto.adam(ts_riverine_flood_corr, model="ZZZ", lags=c(1,12), orders=list(ar=c(3,3), i=(2),
                                                                                      ma=c(3,3), select=TRUE))
fitadam3
summary(fitadam3)

par(mfcol=c(2,2))
plot(fitadam3)

par(mfcol=c(1,1))
plot(fitadam3$states)
plot(fitadam3$residuals)

prev_AES <- forecast(fitadam3,12)
show(prev_AES)
plot(prev_AES)

#### SSARIMA ----

fit_SSARIMA <- auto.ssarima(ts_riverine_flood_corr, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_SSARIMA

par(mfcol=c(2,2))
plot(fit_SSARIMA)

plot(fit_SSARIMA$residuals)

prev_SSARIMA <- forecast(fit_SSARIMA, h=12)
prev_SSARIMA
plot(prev_SSARIMA)

#-- 5. 1. 3. Modèle SARIMA(p, d, q)(P, D, Q)[12] ----------

fit_sarima <- auto.arima(ts_riverine_flood_corr, seasonal = TRUE)

# Affichage du modèle
summary(fit_sarima)
plot(fit_sarima$residuals)

# Prévision sur 12 périodes
prev_SARIMA <- forecast(fit_sarima, h=12)

# Affichage des prévisions

plot(prev_SARIMA)


# Affichage ----

ts_freq_mens_cinema_2021 <- ts(ts_riverine_flood_corr, 
                               start = c(2023, 02), end =c(2024, 01),
                               frequency = 12)

library(ggplot2)
library(dplyr)
library(zoo)
library(scales)  # Pour formater l'axe des dates

# Fonction mise à jour : transforme la date en format mois
extract_forecast_df <- function(fcast_obj, name) {
  df <- data.frame(
    date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
    value = as.numeric(fcast_obj$mean),
    model = name
  )
  df %>% slice_head(n = 12)
}

# Appliquer la fonction à chaque objet de prévision
dfs <- list(
  extract_forecast_df(prev_ADAM_ETS, "ADAM_ETS"),
  extract_forecast_df(prev_AES, "AES"),
  extract_forecast_df(prevets, "ETS"),
  extract_forecast_df(fit, "fit"),
  extract_forecast_df(prev_SARIMA, "SARIMA"),
  extract_forecast_df(prev_SSARIMA, "SSARIMA"),
  extract_forecast_df(prevstl, "STL"),
  extract_forecast_df(prevsts, "STS"),
  extract_forecast_df(prev_TBATS, "TBATS"),
  extract_forecast_df(forecast_x13, "X13")
)

# Regrouper tous les modèles ensemble
df_all <- bind_rows(dfs)

# Transformer la série temporelle en data frame pour ggplot
ts_riverine_flood_corr_df <- data.frame(
  date = as.Date(time(ts_freq_mens_cinema_2021)),
  value = as.numeric(ts_freq_mens_cinema_2021),
  model = "Observed Data"
)

# Affichage avec mois formatés
ggplot() +
  geom_line(data = df_all, aes(x = date, y = value, color = model), size = 1) +
  geom_line(data = ts_riverine_flood_corr_df, aes(x = date, y = value),
            linetype = "dashed", color = "black", size = 1) +
  labs(title = "Prévisions sur 12 mois par modèle",
       x = "Mois", y = "Valeur prévue", color = "Modèle") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

## Qualité de prévision ----

## MSE & R²OOS

# Exemple de données (à remplacer par vos données réelles)
actual_values <- ts_freq_mens_cinema_2021

# Liste des prévisions pour chaque modèle
forecasts <- list(
  prev_ADAM_ETS$mean,
  prev_AES$mean,
  prevets$mean,
  fit$mean,
  prev_SARIMA$mean,
  prev_SSARIMA$mean,
  prevstl$mean,
  prevsts$mean,
  prev_TBATS$mean,
  forecast_x13$mean
)

# Fonction pour calculer MSE et R²OOS
calculate_metrics <- function(actual, forecast, benchmark_forecast) {
  mse <- mean((actual - forecast)^2)
  sst <- sum((actual - benchmark_forecast)^2) # on choisit le meilleur modèle par rapport à STL **
  sse <- sum((actual - forecast)^2)
  r2oos <- 1 - (sse / sst)
  return(list(mse = mse, r2oos = r2oos))
}

# Calculer les métriques pour chaque modèle
metrics <- lapply(forecasts, function(fcast) {
  calculate_metrics(as.numeric(actual_values), as.numeric(fcast), as.numeric(prevstl$mean)) ## **
})

# Convertir en data frame pour une meilleure lisibilité
metrics_df <- as.data.frame(do.call(rbind, metrics))
rownames(metrics_df) <- c("ADAM_ETS", "AES", "ETS", "fit", "SARIMA", "SSARIMA", "STL", "STS", "TBATS", "X13")

print(metrics_df)

# le meilleur modèle en terme de prévision et d'ajustement est SARIMA
