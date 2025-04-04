library(readxl)
castrophes_naturelles <- read_excel("castrophes naturelles.xlsx")

df <- data.frame(castrophes_naturelles)

library(tidyverse)


list_types <- df |> 
  group_by(Disaster.Type) |> 
  group_split()

print(list_types)

# Création de variables séparées pour chaque sous-groupe

for (i in seq_along(list_types)) {
  # Le nom de chaque dataframe sera basé sur la valeur de Disaster.Type
  assign(paste0("df_", unique(list_types[[i]]$Disaster.Type)), list_types[[i]])
}


library(dplyr)

# Liste des noms des dataframes créés
df_names <- ls(pattern = "^df_")

# Pour chaque dataframe, on applique count() et arrange()
for (df_name in df_names) {
  # On récupère le dataframe en utilisant get()
  df <- get(df_name)
  
  # Appliquer count() et arrange() sur le dataframe
  result <- df |>
    count(Country) |>
    arrange(desc(n))
  
  # Afficher le résultat ou l'assigner à une nouvelle variable
  print(paste("Résultats pour", df_name))
  print(result)
}

# Je ne garde que les df avec suffisament d'observations
# suffisament d'observation dans un seul pays 
# suffisament de pays
# suffisament de pays avec des données accessible facilement

# df_Earthquake
# df_Extreme temperature
# df_Flood
# df_Mass movement (wet)
# df_Storm
# df_Wildfire


list_keep <- list(df_Earthquake, `df_Extreme temperature`, df_Flood,
                  `df_Mass movement (wet)`, df_Storm, df_Wildfire)

list_sub_df <- c()

for (i in seq_along(list_keep)) {
  # Le nom de chaque dataframe sera basé sur la valeur de Disaster.Type
  unique_subtypes <- unique(list_keep[[i]]$Disaster.Subtype)
  
  # Boucle pour chaque valeur unique de 'Disaster.Subtype'
  for (subtype in unique_subtypes) {
    
    # Créer un nom pour la variable en fonction du 'Disaster.Subtype'
    df_name <- paste0("sub_df_", subtype)
    
    # Filtrer les lignes correspondant à ce 'Disaster.Subtype'
    df_filtered <- list_keep[[i]] %>% filter(Disaster.Subtype == subtype)
    
    # Assigner ce dataframe filtré à une nouvelle variable
    assign(df_name, df_filtered)
    
    list_sub_df <- append(list_sub_df, df_name)
  }
}





