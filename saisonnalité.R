library(TSA)
library(ggplot2)
library(forecast)
library(tsoutliers)
library(dplyr)
library(tidyverse)
library(anomalize)
library(tibbletime)
library(tidyquant)

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

## dico_ts_acceptes[[1]] = ground_movement ----

outliers_hail <- tso(dico_ts_acceptes[[1]]) 
print(outliers_hail)
plot(outliers_hail)

# trouver l'origine de cette valeur atypique
window(dico_ts_acceptes[[1]], start = c(2012, 1), end = c(2014, 12))
plot(window(dico_ts_acceptes[[1]], start = c(2012, 1), end = c(2014, 12)))
# C'est un séisme de magnitude 6.6 du 20 avril 2013

## Correction ----
ts_ground_movement_corr <- outliers_hail$yadj

par(mfcol=c(1,2))
plot(ts_ground_movement_corr)
plot(dico_ts_acceptes[[1]])



## dico_ts_acceptes[[3]] = Cold wave ----

outliers_hail <- tso(dico_ts_acceptes[[3]]) 
print(outliers_hail)
plot(outliers_hail)

### --------- Problème ---------- ## ----

## tsoutliers::tso() repose sur un modèle ARIMA ajusté,
## or ce type de séries ne se prête pas toujours bien à ce genre de modélisation, car :
## L’essentiel de la série est à 0, donc auto.arima() abandonne ou donne un modèle non pertinent.

decomp_Cold <- decompose(dico_ts_acceptes[[3]])   
plot(decomp_Cold)

par(mfcol=c(1,1))
dico_ts_acceptes[[3]]|> 
  boxplot()

## dico_ts_acceptes[[4]] = heat wave ----

outliers_heat <- tso(dico_ts_acceptes[[4]]) 
print(outliers_heat)
plot(outliers_heat)

### pas de valeurs atypiques ----

ts_heat_wave <- dico_ts_acceptes[[4]]

## dico_ts_acceptes[[5]] = Severe winter conditions ----

outliers_Severe_winter <- tso(dico_ts_acceptes[[5]]) 
print(outliers_Severe_winter)
plot(outliers_Severe_winter)

### --------- Problème ---------- ## ----

decomp_winter <- decompose(dico_ts_acceptes[[5]])   
plot(decomp_winter)

par(mfcol=c(1,1))
dico_ts_acceptes[[5]]|> 
  boxplot()
## dico_ts_acceptes[[6]] = Riverine flood ----

outliers_riverine <- tso(dico_ts_acceptes[[6]]) 
print(outliers_riverine)
plot(outliers_riverine)

# trouver l'origine de cette valeur atypique
window(dico_ts_acceptes[[6]], start = c(2015, 1), end = c(2020, 12))
plot(window(dico_ts_acceptes[[6]], start = c(2015, 1), end = c(2020, 12)))

### Correction ----
ts_riverine_flood_corr <- outliers_riverine$yadj

par(mfcol=c(1,2))
plot(ts_riverine_flood_corr)
plot(dico_ts_acceptes[[6]])

#### Faire attention aux graphs j'ai des valeurs négatives ----

## dico_ts_acceptes[[7]] = Flash flood ----

outliers_Flash <- tso(dico_ts_acceptes[[7]]) 
print(outliers_Flash)
plot(outliers_Flash)

### Correction ----
ts_Flash_flood_corr <- outliers_Flash$yadj

par(mfcol=c(1,2))
plot(ts_Flash_flood_corr)
plot(dico_ts_acceptes[[7]])

## dico_ts_acceptes[[9]] =  flood ----

outliers_Flood <- tso(dico_ts_acceptes[[9]]) 
print(outliers_Flood)
plot(outliers_Flood)

### --------- Problème ---------- ## ----

decomp_flood <- decompose(dico_ts_acceptes[[9]])   
plot(decomp_flood)

par(mfcol=c(1,1))
dico_ts_acceptes[[9]]|> 
  boxplot()

## dico_ts_acceptes[[10]] =  Landslide (wet) ----

outliers_Landslide <- tso(dico_ts_acceptes[[10]]) 
print(outliers_Landslide)
plot(outliers_Landslide)

### Correction ----
ts_Landslide_wet_corr <- outliers_Landslide$yadj

par(mfcol=c(1,2))
plot(ts_Landslide_wet_corr)
plot(dico_ts_acceptes[[10]])

## dico_ts_acceptes[[12]] = Avalanche (wet) ----

outliers_Avalanche <- tso(dico_ts_acceptes[[12]]) 
print(outliers_Avalanche)
plot(outliers_Avalanche)

### pas de valeurs atypiques ----

ts_Avalanche_wet <- dico_ts_acceptes[[12]]

## dico_ts_acceptes[[15]] = Storm ----

outliers_Storm <- tso(dico_ts_acceptes[[15]]) 
print(outliers_Storm)
plot(outliers_Storm)

### --------- Problème ---------- ## ----

decomp_Storm <- decompose(dico_ts_acceptes[[15]])   
plot(decomp_Storm)

par(mfcol=c(1,1))
dico_ts_acceptes[[15]]|> 
  boxplot()

## dico_ts_acceptes[[16]] = Tornado ----

outliers_Tornado <- tso(dico_ts_acceptes[[16]]) 
print(outliers_Tornado)
plot(outliers_Tornado)

### Correction ----
ts_Tornado_corr <- outliers_Tornado$yadj

par(mfcol=c(1,2))
plot(ts_Tornado_corr)
plot(dico_ts_acceptes[[16]])

#### Faire attention aux graphs j'ai des valeurs négatives ----

## dico_ts_acceptes[[17]] = Blizzard ----

outliers_Blizzard <- tso(dico_ts_acceptes[[17]]) 
print(outliers_Blizzard)
plot(outliers_Blizzard)

### --------- Problème ---------- ## ----

decomp_Tornado <- decompose(dico_ts_acceptes[[16]])   
plot(decomp_Tornado)

par(mfcol=c(1,1))
dico_ts_acceptes[[18]]|> 
  boxplot()

## dico_ts_acceptes[[18]] = Tropical cyclone ----

outliers_Tropical <- tso(dico_ts_acceptes[[18]]) 
print(outliers_Tropical)
plot(outliers_Tropical)

### Correction ----
ts_Tropical_cyclone_corr <- outliers_Tropical$yadj

par(mfcol=c(1,2))
plot(ts_Tropical_cyclone_corr)
plot(dico_ts_acceptes[[18]])

#### Faire attention aux graphs j'ai des valeurs négatives ----

## dico_ts_acceptes[[19]] = Hail ----

outliers_Hail <- tso(dico_ts_acceptes[[19]]) 
print(outliers_Hail)
plot(outliers_Hail)

### pas de valeurs atypiques ----

ts_Hail <- dico_ts_acceptes[[19]]

## dico_ts_acceptes[[20]] = Severe weather ----

outliers_Severe_weather <- tso(dico_ts_acceptes[[20]]) 
print(outliers_Severe_weather)
plot(outliers_Severe_weather)

### Correction ----
ts_Severe_weather_corr <- outliers_Severe_weather$yadj

par(mfcol=c(1,2))
plot(ts_Severe_weather_corr)
plot(dico_ts_acceptes[[20]])

#### Faire attention aux graphs j'ai des valeurs négatives ----

## dico_ts_acceptes[[21]] = Thunderstorms ----

outliers_Thunderstorms <- tso(dico_ts_acceptes[[21]]) 
print(outliers_Thunderstorms)
plot(outliers_Thunderstorms)

### Correction ----
ts_Thunderstorms_corr <- outliers_Thunderstorms$yadj

par(mfcol=c(1,2))
plot(ts_Thunderstorms_corr)
plot(dico_ts_acceptes[[21]])

## dico_ts_acceptes[[23]] = Extra-tropical ----

outliers_Extra_tropical <- tso(dico_ts_acceptes[[23]]) 
print(outliers_Extra_tropical)
plot(outliers_Extra_tropical)

### Correction ----
ts_Extra_tropical_corr <- outliers_Extra_tropical$yadj

par(mfcol=c(1,2))
plot(ts_Extra_tropical_corr)
plot(dico_ts_acceptes[[23]])

## dico_ts_acceptes[[27]] = Forest fire ----

outliers_Forest_fire <- tso(dico_ts_acceptes[[27]]) 
print(outliers_Forest_fire)
plot(outliers_Forest_fire)

### Correction ----
ts_Forest_fire_corr <- outliers_Forest_fire$yadj

par(mfcol=c(1,2))
plot(ts_Forest_fire_corr)
plot(dico_ts_acceptes[[27]])

#### Faire attention aux graphs j'ai des valeurs négatives ----

## dico_ts_acceptes[[28]] = Wildfire ----

outliers_Wildfire <- tso(dico_ts_acceptes[[28]]) 
print(outliers_Wildfire)
plot(outliers_Wildfire)

### Correction ----
ts_Wildfire_corr <- outliers_Wildfire$yadj

par(mfcol=c(1,2))
plot(ts_Wildfire_corr)
plot(dico_ts_acceptes[[28]])

#### Faire attention aux graphs j'ai des valeurs négatives ----
