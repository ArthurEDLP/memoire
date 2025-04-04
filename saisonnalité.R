library(TSA)
library(ggplot2)
library(forecast)
library(tsoutliers)
library(dplyr)

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



