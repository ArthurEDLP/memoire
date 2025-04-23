library(ggplot2)       
library(forecast)      
library(tsoutliers)    
library(dplyr)         
library(tidyr)         
library(zoo)           
library(seastests)

# transforamtion en ts ----

dico_ts_acceptes <- list()  
liste_acceptes <- c() 
liste_sub_pas_acceptes <- c() 

for (sub in seq_along(list_sub_df)) {
  
  # Récupérer le dataframe à partir de son nom dans list_sub_df
  df_name <- list_sub_df[[sub]]  # Récupère le nom du dataframe (string)
  df <- get(df_name)  # Récupère le dataframe réel
  
  # Vérifier que le dataframe a au moins 50 observations
  if (nrow(df) >= 50) {  
    df_Cold <- df |> 
      mutate(
        Start.Month = coalesce(Start.Month, End.Month),  # Remplace NA par End.Month
        Date = paste0(Start.Year, "-", sprintf("%02d", Start.Month))
      ) |> 
      count(Date) |> 
      arrange(Date)
    
    if (!all(is.na(df_Cold$Date))) {
      df_Cold <- df_Cold |> 
        mutate(Date = as.Date(paste0(Date, "-01"))) |> 
        complete(Date = seq(min(Date, na.rm = TRUE), max(Date, na.rm = TRUE), by = "month"), fill = list(n = 0))
    }
    
    Cold_ts <- ts(df_Cold$n, start = c(2000, 1), frequency = 12)
    
    # Décomposer et afficher le graphique
    decomp_Cold <- decompose(Cold_ts)   
    plot(decomp_Cold)
    title(main = df_name)  # Ajouter un titre après le plot
    
    # Ajouter la série ts au dictionnaire
    dico_ts_acceptes[[sub]] <- Cold_ts
    liste_acceptes <- append(liste_acceptes, df_name)
    
  } else {
    message(paste("⚠️", df_name, "a moins de 50 observations et sera ignoré."))
    
    liste_sub_pas_acceptes <- append(liste_sub_pas_acceptes, df_name)
  }
}


# valeurs atypiques -----

## pas de problème de valeurs atypique car ici c'est normal

# on ne va garder que les séries avec des données allant jusqu'en 2024, (càd comprenant tout 2023)

ts_ground_movement <- dico_ts_acceptes[[1]] # s'arrête à 2025
ts_Cold_wave <- dico_ts_acceptes[[3]] # s'arrête à 2024
ts_heat_wave <- dico_ts_acceptes[[4]] # s'arrête à 2024
ts_severe_winter_conditions <- dico_ts_acceptes[[5]] # s'arrête à 2017
ts_riverine_flood <- dico_ts_acceptes[[6]]  # s'arrête à 2023
ts_flash_flood <- dico_ts_acceptes[[7]]  # s'arrête à 2025
ts_flood <- dico_ts_acceptes[[9]]  # s'arrête à 2025
ts_landslide_wet <- dico_ts_acceptes[[10]]  # s'arrête à 2025
ts_avalanche_wet <- dico_ts_acceptes[[12]]  # s'arrête à 2025
ts_storm <- dico_ts_acceptes[[15]]  # s'arrête à 2025
ts_tornado <- dico_ts_acceptes[[16]]  # s'arrête à 2024
ts_blizzard <- dico_ts_acceptes[[17]]  # s'arrête à 2025
ts_tropical_cyclone <- dico_ts_acceptes[[18]]  # s'arrête à 2025
ts_hail <- dico_ts_acceptes[[19]]  # s'arrête à 2024
ts_severe_weather <- dico_ts_acceptes[[20]]  # s'arrête à 2024
ts_thunderstorms <- dico_ts_acceptes[[21]]  # s'arrête à 2023
ts_extra_tropical <- dico_ts_acceptes[[23]]  # s'arrête à 2021
ts_forest_fire <- dico_ts_acceptes[[27]]  # s'arrête à 2025
ts_wildfire <- dico_ts_acceptes[[28]]  # s'arrête à 2018

liste_des_séries <- c(
  "ts_ground_movement", 
  "ts_Cold_wave", 
  "ts_heat_wave", 
  "ts_flash_flood", 
  "ts_flood", 
  "ts_landslide_wet", 
  "ts_avalanche_wet", 
  "ts_storm", 
  "ts_tornado", 
  "ts_blizzard", 
  "ts_tropical_cyclone", 
  "ts_hail", 
  "ts_severe_weather", 
  "ts_forest_fire"
)

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



colnames(castrophes_naturelles) #Disaster Subtype


library(dplyr)

# Liste des sous-types de désastres à conserver
sous_types <- c(
  "Ground movement", "Cold wave", "Heat wave", 
  "Flash flood", "Flood (General)", "Landslide (wet)", 
  "Avalanche (wet)", "Storm (General)", "Tornado", 
  "Blizzard/Winter storm", "Tropical cyclone", "Hail", 
  "Severe weather", "Forest fire"
)

# Filtrage de la base de données
catastrophes_filtrees <- castrophes_naturelles %>%
  filter(`Disaster Subtype` %in% sous_types)

df_catastrophes_filtrees <- data.frame(catastrophes_filtrees)

df_catastrophes_filtrees <- df_catastrophes_filtrees |>
  mutate(
    Start.Month = coalesce(Start.Month, End.Month),  # Remplace NA par End.Month
    Date = paste0(Start.Year, "-", sprintf("%02d", Start.Month))
  ) |>
  group_by(Date) |>
  summarize(count = n()) |>
  arrange(Date)



ts_catastrophes_filtrees_2000_2025 <- ts(df_catastrophes_filtrees$count, start = c(2000, 1), frequency = 12)

plot(ts_catastrophes_filtrees_2000_2025)

ts_catastrophes_2020_2023 <- ts(df_catastrophes_filtrees$count, start = c(2000, 1), end = c(2023, 12), frequency = 12)
# on s'arrête en 2023 car toutes les données ne vont pas jusqu'en 2025, 
# mais elles vont toutes jusqu'en 2023


plot(ts_catastrophes_2020_2023)








