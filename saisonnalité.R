library(TSA)

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

# doute sur 4 séries je fais des tests supplémentaires ----
## ts_flood

month <- cycle(ts_flood)
friedman.test(matrix(ts_flood, ncol = 12, byrow = TRUE))


auto.arima(ts_flood, seasonal = FALSE)  # sans saisonnalité
auto.arima(ts_flood, seasonal = TRUE)   # avec saisonnalité


decompo <- stl(ts_flood, s.window = "periodic")
plot(decompo)

## ts_avalanche_wet

month <- cycle(ts_avalanche_wet)
friedman.test(matrix(ts_avalanche_wet, ncol = 12, byrow = TRUE))


auto.arima(ts_avalanche_wet, seasonal = FALSE)  # sans saisonnalité
auto.arima(ts_avalanche_wet, seasonal = TRUE)   # avec saisonnalité


decompo <- stl(ts_avalanche_wet, s.window = "periodic")
plot(decompo)

## ts_flash_flood

month <- cycle(ts_flash_flood)
friedman.test(matrix(ts_flash_flood, ncol = 12, byrow = TRUE))


auto.arima(ts_flash_flood, seasonal = FALSE)  # sans saisonnalité
auto.arima(ts_flash_flood, seasonal = TRUE)   # avec saisonnalité


decompo <- stl(ts_flash_flood, s.window = "periodic")
plot(decompo)

## ts_hail

month <- cycle(ts_hail)
friedman.test(matrix(ts_hail, ncol = 12, byrow = TRUE))


auto.arima(ts_hail, seasonal = FALSE)  # sans saisonnalité
auto.arima(ts_hail, seasonal = TRUE)   # avec saisonnalité


decompo <- stl(ts_hail, s.window = "periodic")
plot(decompo)

# Conclusion:
## ts_avalanche_wet saisonnier: p-value significative
## flood non saisonnier : p-value ok mais AIC identique
## hail saisonnier : p-value ok mais AIC identique, STL pas convaincant
## flash_flood saisonnier: p-value significative et différence d'AIC



# Saisonnière:
saison <- c(
  "ts_Cold_wave", 
  "ts_heat_wave", 
  "ts_flash_flood", 
  "ts_avalanche_wet", 
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
  "ts_flood", 
  "ts_landslide_wet",
  "ts_storm"
)


# saisonnalité sur la base principale ----

month <- cycle(ts_catastrophes_2000_2023)
print(kruskal.test(as.numeric(ts_catastrophes_2000_2023) ~ factor(month)))
# p-value = 2.834e-07

print(combined_test(ts_catastrophes_2000_2023))
# 6.816589e-09

dyy <- diff(ts_catastrophes_2000_2023, differences = 1)
periodogram(dyy, main = "Periodogramme sur la série en différence première catastrophes_2020_2023")

# c'est saisonnier.