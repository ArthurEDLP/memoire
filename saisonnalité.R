library(TSA)
library(ggplot2)
library(forecast)


library(dplyr)

df_test_Storm <- df_Storm |> 
  mutate(Date = paste0(Start.Year, "-", sprintf("%02d", Start.Month))) |> 
  count(Date) |> 
  arrange(Date)

# Convertir en format date pour mieux gérer les mois manquants
df_test_Storm <- df_test_Storm |> 
  mutate(Date = as.Date(paste0(Date, "-01"))) |> 
  complete(Date = seq(min(Date), max(Date), by = "month"), fill = list(n = 0))


print(df_test_Storm)


Storm_ts <- ts(df_test_Storm$n, start = c(2000, 1), frequency = 12)

decomp_Storm <- decompose(Storm_ts)   
plot(decomp_Storm)

###################################

df_Cold <- `sub_df_Cold wave` |> 
  mutate(Date = paste0(Start.Year, "-", sprintf("%02d", Start.Month))) |> 
  count(Date) |> 
  arrange(Date)

df_Cold <- df_Cold |> 
  mutate(Date = as.Date(paste0(Date, "-01"))) |> 
  complete(Date = seq(min(Date), max(Date), by = "month"), fill = list(n = 0))

print(df_Cold)


Cold_ts <- ts(df_Cold$n, start = c(2000, 1), frequency = 12)

decomp_Cold <- decompose(Cold_ts)   
plot(decomp_Cold)


#####################

list_sub_acceptés <- c()
list_sub_pas_acceptés <- c()


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
    
    decomp_Cold <- decompose(Cold_ts)   
    plot(decomp_Cold)
    title(main = df_name)  # Ajouter un titre après le plot
    list_sub_acceptés <- append(list_sub_acceptés, df_name) # liste des df a analysé
  } else {
    message(paste("⚠️", df_name, "a moins de 50 observations et sera ignoré."))
    list_sub_pas_acceptés <- append(list_sub_pas_acceptés, df_name)
  }
}
