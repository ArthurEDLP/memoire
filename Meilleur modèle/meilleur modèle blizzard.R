library(EnvStats)
library(moments)
library(tibble)
library(seasonal)
library(RJDemetra)
library(smooth)
library(gridExtra)
library(scales)


ts_blizzard_2022 <- ts(ts_blizzard, start = c(2000, 1), end = c(2022, 12), frequency = 12)



# Saisonnalité  ----

ts_blizzard_2022 |> 
  decompose(, type = "additive") |> 
  plot()

ts_blizzard_2022 |> 
  decompose(, type = "multiplicative") |> 
  plot()


# Test saisonnalité

summary(regarima_x13(ts_blizzard_2022, spec ="RG5c"))

# Additive


# Estimer et prévoir ----

#-- 5. 1. 1. Les premières méthodes ----------

#### naïves ----
par(mfcol=c(1,1))

pred_naive <- naive(ts_blizzard_2022, h=12)
show(pred_naive)
plot(pred_naive)

####  StructTS ----

fitsts = StructTS(ts_blizzard_2022)
plot(cbind(fitted(fitsts), residuals(fitsts)))
show(fitsts)

summary(fitsts)
prevsts <- forecast(fitsts, 12) 
show(prevsts) 
plot(prevsts)



#### stlm ----

decomp = stl(ts_blizzard_2022, s.window="periodic")
# show(decomp)
plot(decomp)

fitstl = stlm(ts_blizzard_2022)

prevstl <- forecast(fitstl,12) #période d'une année

# show(prevsts) # pas mettre en annexe

plot(prevstl) # en annexe

summary(prevstl)



#### X13 ----

myspec <- x13_spec("RSA5c")

mysax13 <- x13(ts_blizzard_2022, myspec)

summary(mysax13$regarima)

mysax13
par(mfcol=c(1,2))
plot(mysax13$final)

par(mfcol=c(1,1))

# Extraire la série désaisonnalisée
sa_series <- mysax13$final$series[, "sa"]

# Appliquer la prévision sur 12 périodes
forecast_x13 <- forecast(sa_series, h = 12)

# Visualiser le résultat
plot(forecast_x13)



#-- 5. 1. 2. Prédiction sur les méthodes de lissage exponentiel ----------


#### Holt-winters ----

# Modèle Holt-Winters (additif par défaut)
fit_hw <- HoltWinters(ts_blizzard_2022, seasonal = "add")
fit_hw

# Calcul de l'AIC
# Calcul des résidus
residuals <- residuals(fit_hw)

# Calcul du MSE
mse <- mean(residuals^2)

# Estimation de la vraisemblance
n <- length(ts_blizzard_2022)  # nombre d'observations
k <- length(coef(fit_hw))  # nombre de paramètres
log_likelihood <- -(n/2) * log(2 * pi * mse) - (1/2) * sum(residuals^2 / mse)


aic_value <- 2 * k - 2 * log_likelihood
print(aic_value)

# Calcul de l'AICc
aicc_value <- aic_value + (2 * k * (k + 1)) / (n - k - 1)
print(aicc_value)

# Prévision sur 12 mois
pred_hw <- forecast(fit_hw, h = 12)

# Affichage
plot(pred_hw)


#### ETS ----

fitets <- ets(ts_blizzard_2022, ic = "aic") # pour avoir le meilleur AIC
show(fitets)
plot(fitets)

plot(fitets$residuals)

prevets <- forecast(fitets,12)
show(prevets)
plot(prevets)


#### TBATS ----

fit_tbats <- tbats(ts_blizzard_2022)
show(fit_tbats)
plot(fit_tbats)

prev_TBATS <- forecast(fit_tbats, h=12)
plot_prev_TBATS <- plot(prev_TBATS)



#### ADAM ETS ----


fit_ADAM_ETS <- auto.adam(ts_blizzard_2022, model = "NAA", lags = c(1, 12), select = TRUE)
fit_ADAM_ETS
# Voir les paramètres estimés
coef(fit_ADAM_ETS)

# Voir les informations du modèle
modelName(fit_ADAM_ETS)

# Résumé rapide manuel :
fit_ADAM_ETS$forecast
fit_ADAM_ETS$residuals
fit_ADAM_ETS$likelihood
fit_ADAM_ETS$ICs


par(mfcol=c(2,2))
plot(fit_ADAM_ETS)

par(mfcol=c(1,1))


plot(fit_ADAM_ETS$states)
plot(fit_ADAM_ETS$residuals)


prev_ADAM_ETS <- forecast(fit_ADAM_ETS, h=12)
show(prev_ADAM_ETS)
plot(prev_ADAM_ETS)

#### ADAM ETS + SARIMA ----

fitadam3 <- auto.adam(ts_blizzard_2022, model = "NAA", lags=c(1,1,12),orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))


fitadam3


par(mfcol=c(2,2))
plot(fitadam3)

par(mfcol=c(1,1))
plot(fitadam3$states)
plot(fitadam3$residuals)

prev_AES <- forecast(fitadam3,12)
show(prev_AES)
plot(prev_AES)

#### SSARIMA ----

fit_SSARIMA <- auto.ssarima(ts_blizzard_2022, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_SSARIMA

par(mfcol=c(2,2))
plot(fit_SSARIMA)

par(mfcol=c(1,1))

plot(fit_SSARIMA$residuals)

prev_SSARIMA <- forecast(fit_SSARIMA, h=12)
prev_SSARIMA
plot(prev_SSARIMA)

#-- 5. 1. 3. Modèle SARIMA(p, d, q)(P, D, Q)[12] ----------

fit_sarima <- auto.arima(ts_blizzard_2022, seasonal = TRUE)

# Affichage du modèle
summary(fit_sarima)
plot(fit_sarima$residuals)

# Prévision sur 12 périodes
prev_SARIMA <- forecast(fit_sarima, h=12) 

# Affichage des prévisions

plot(prev_SARIMA)

#-- 5. 2.  le meilleur modèle d’après les critères AIC et AICc ----------




#---------- 6. Représenter graphiquement l’évolution des prévisions des différents modèles ----------

ts_blizzard_2023 <- ts(ts_blizzard_2022, 
                      start = c(2023, 1), end =c(2023, 12),
                      frequency = 12)


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
  extract_forecast_df(pred_hw , "HW"),
  extract_forecast_df(prev_SARIMA, "SARIMA"),
  extract_forecast_df(prev_SSARIMA, "SSARIMA"),
  extract_forecast_df(prevstl, "STL"),
  extract_forecast_df(prevsts, "STS"),
  extract_forecast_df(prev_TBATS, "TBATS"),
  extract_forecast_df(forecast_x13, "X13"),
  extract_forecast_df(pred_naive, "NAÏVE")
  
)

# Regrouper tous les modèles ensemble
df_all <- bind_rows(dfs)

# Transformer la série temporelle en data frame pour ggplot
ts_blizzard_2023_df <- data.frame(
  date = as.Date(time(ts_blizzard_2023)),
  value = as.numeric(ts_blizzard_2023),
  model = "Observed Data"
)

# Affichage avec mois formatés
ggplot() +
  geom_line(data = df_all, aes(x = date, y = value, color = model), size = 1) +
  geom_line(data = ts_blizzard_2023_df, aes(x = date, y = value),
            linetype = "dashed", color = "black", size = 1) +
  labs(title = "Prévisions sur 12 mois par modèle",
       x = "Mois", y = "Valeur prévue", color = "Modèle") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


#---------- 7. Qualité de prévision ----------

## MSE & R²OOS

# Exemple de données (à remplacer par vos données réelles)
actual_values <- ts_blizzard_2023

# Liste des prévisions pour chaque modèle
forecasts <- list(
  prev_ADAM_ETS$mean,
  prev_AES$mean,
  prevets$mean,
  pred_hw$mean,
  prev_SARIMA$mean,
  prev_SSARIMA$mean,
  prevstl$mean,
  prevsts$mean,
  prev_TBATS$mean,
  forecast_x13$mean,
  pred_naive$mean
)

# Fonction pour calculer MSE et R²OOS
calculate_metrics <- function(actual, forecast, benchmark_forecast) {
  mse <- mean((actual - forecast)^2)
  sst <- sum((actual - benchmark_forecast)^2)
  sse <- sum((actual - forecast)^2)
  r2oos <- 1 - (sse / sst)
  return(list(mse = mse, r2oos = r2oos))
}

# Calculer les métriques pour chaque modèle
metrics <- lapply(forecasts, function(fcast) {
  calculate_metrics(as.numeric(actual_values), as.numeric(fcast), as.numeric(prevstl$mean))
})

# Convertir en data frame pour une meilleure lisibilité
metrics_df <- as.data.frame(do.call(rbind, metrics))
rownames(metrics_df) <- c("ADAM_ETS", "AES", "ETS", "HW", "SARIMA", "SSARIMA", "STL", "STS", "TBATS", "X13", "NAÏVE")
print(metrics_df)

## CSPE

# Fonction pour calculer les CSPE
calculate_cspe <- function(actual, forecast) {
  errors <- (actual - forecast)^2
  cspe <- cumsum(errors)
  return(cspe)
}

# Calculer les CSPE pour chaque modèle
cspe_list <- lapply(forecasts, function(fcast) {
  calculate_cspe(as.numeric(actual_values), as.numeric(fcast))
})

# Convertir en data frame pour le traçage
cspe_df <- do.call(cbind, cspe_list)
colnames(cspe_df) <- c("ADAM_ETS", "AES", "ETS", "HW", "SARIMA", "SSARIMA", "STL", "STS", "TBATS", "X13", "NAÏVE")
cspe_df <- cbind(Date = as.Date(time(actual_values)), cspe_df)


# Convertir la matrice en data frame
cspe_df <- as.data.frame(cspe_df)

# Vérifier la structure du data frame
str(cspe_df)

# Utiliser pivot_longer pour transformer le data frame
cspe_df_long <- pivot_longer(cspe_df, cols = -Date, names_to = "Model", values_to = "CSPE")

# Tracer les CSPE
ggplot(cspe_df_long, aes(x = Date, y = CSPE, color = Model)) +
  geom_line() +
  labs(title = "Cumulative Squared Prediction Errors (CSPE)",
       x = "Date", y = "CSPE", color = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

