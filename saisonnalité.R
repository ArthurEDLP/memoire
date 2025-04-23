# tests de saisonnalité: ----
for (sub in seq_along(liste_des_séries)) {
  
  df_name <- liste_des_séries[[sub]]  # Récupère le nom du dataframe (string)
  df <- get(df_name)
  
  month <- cycle(df)
  print(df_name)
  print(kruskal.test(as.numeric(df) ~ factor(month)))
  print(combined_test(df))
  
  dyy <- diff(df, differences = 1)
  periodogram(dyy, main=paste("Periodogramme sur la série en différence première", df_name))
  
  
}

# doute sur 3 séries je fais des tests supplémentaires ----
## ts_flood

month <- cycle(ts_flood)
friedman.test(matrix(ts_flood, ncol = 12, byrow = TRUE))


library(forecast)
auto.arima(ts_flood, seasonal = FALSE)  # sans saisonnalité
auto.arima(ts_flood, seasonal = TRUE)   # avec saisonnalité


decompo <- stl(ts_flood, s.window = "periodic")
plot(decompo)

## ts_landslide_wet

month <- cycle(ts_landslide_wet)
friedman.test(matrix(ts_landslide_wet, ncol = 12, byrow = TRUE))


auto.arima(ts_landslide_wet, seasonal = FALSE)  # sans saisonnalité
auto.arima(ts_landslide_wet, seasonal = TRUE)   # avec saisonnalité


decompo <- stl(ts_landslide_wet, s.window = "periodic")
plot(decompo)

## ts_storm

month <- cycle(ts_storm)
friedman.test(matrix(ts_storm, ncol = 12, byrow = TRUE))


auto.arima(ts_storm, seasonal = FALSE)  # sans saisonnalité
auto.arima(ts_storm, seasonal = TRUE)   # avec saisonnalité


decompo <- stl(ts_storm, s.window = "periodic")
plot(decompo)

# Conclusion:

## Même si les glissements de terrain sont liés à la météo,
## leur comportement n'est pas assez saisonnier pour être traité comme des feux de forêts.


# Saisonnière:
saison <- c(
  "ts_Cold_wave", 
  "ts_heat_wave", 
  "ts_flash_flood", 
  "ts_flood", 
  "ts_avalanche_wet", 
  "ts_storm", 
  "ts_tornado", 
  "ts_blizzard", 
  "ts_tropical_cyclone", 
  "ts_hail", 
  "ts_severe_weather", 
  "ts_forest_fire"
)

# Non Saisonnière:

non_saison <- c(
  "ts_ground_movement", 
  "ts_landslide_wet"
)



