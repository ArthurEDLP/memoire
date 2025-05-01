# Charger les packages
library(dplyr)
library(tidyr)
library(readr)
library(openxlsx)

# 1. CHARGER TES BASES

# Exemples de chargement (ajuste si besoin)
# df_pop_monde_pays_20_23 <- read_csv("ton_chemin/df_pop_monde_pays_20_23.csv")
# df_temp_pays_20_23 <- read_csv("ton_chemin/df_temp_pays_20_23.csv")
# df_CO2_20_23 <- read_csv("ton_chemin/df_CO2_20_23.csv")
# catastrophes_filtrees <- read_excel("ton_chemin/catastrophes_filtrees.xlsx")


# 2. HARMONISER LES NOMS DE PAYS

correspondance_pays <- c(
  "United States" = "United States of America",
  "US" = "United States of America",
  "Russia" = "Russian Federation",
  "Czech Republic" = "Czechia",
  "North Korea" = "Democratic People's Republic of Korea",
  "South Korea" = "Republic of Korea",
  "Ivory Coast" = "Côte d’Ivoire",
  "Myanmar (Burma)" = "Myanmar",
  "Equador" = "Ecuador",
  "Serbia-Montenegro" = "Serbia Montenegro",
  "Yugoslavia" = "Serbia Montenegro",
  "The Netherlands" = "Netherlands (Kingdom of the)",
  "Hong Kong" = "China, Hong Kong Special Administrative Region",
  "Taiwan" = "Taiwan (Province of China)"
)

df_pop_monde_pays_20_23$Country <- recode(df_pop_monde_pays_20_23$Country, !!!correspondance_pays)
df_temp_pays_20_23$Country <- recode(df_temp_pays_20_23$Country, !!!correspondance_pays)
df_CO2_20_23$Country <- recode(df_CO2_20_23$Country, !!!correspondance_pays)
catastrophes_filtrees$Country <- recode(catastrophes_filtrees$Country, !!!correspondance_pays)

# 3. PRÉPARER LES DONNÉES

# Catastrophes totales par pays et année
catastrophes_totales <- catastrophes_filtrees %>%
  group_by(Country, `Start Year`) %>%
  summarise(Catastrophes_totales = n(), .groups = "drop") %>%
  rename(Year = `Start Year`)

# Sous-types spécifiques
sous_types <- c(
  "Ground movement", "Cold wave", "Heat wave", "Flash flood", "Flood (General)",
  "Landslide (wet)", "Avalanche (wet)", "Storm (General)", "Tornado",
  "Blizzard/Winter storm", "Tropical cyclone", "Hail", "Severe weather", "Forest fire"
)

catastrophes_soustypes <- catastrophes_filtrees %>%
  filter(`Disaster Subtype` %in% sous_types) %>%
  group_by(Country, `Start Year`, `Disaster Subtype`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = `Disaster Subtype`, values_from = Count, values_fill = 0) %>%
  rename(Year = `Start Year`)

# 4. DÉFINIR LES PAYS ET ANNÉES COMMUNS

pays_communs <- Reduce(intersect, list(
  unique(df_pop_monde_pays_20_23$Country),
  unique(df_temp_pays_20_23$Country),
  unique(df_CO2_20_23$Country),
  unique(catastrophes_filtrees$Country)
))

annees_communes <- 2000:2020  # limité à 2020 car température seulement jusqu'à 2020

# DIAGNOSTIC RAPIDE
cat("Nombre de pays communs :", length(pays_communs), "\n")
cat("Liste des pays communs :\n")
print(pays_communs)

# 5. CONSTRUIRE LA GRILLE COMPLÈTE

grille_finale <- expand.grid(
  Country = pays_communs,
  Year = annees_communes
)

# 6. FUSIONNER

df_fusion <- grille_finale %>%
  left_join(df_pop_monde_pays_20_23, by = c("Country", "Year")) %>%
  left_join(df_temp_pays_20_23, by = c("Country", "Year")) %>%
  left_join(df_CO2_20_23, by = c("Country", "Year")) %>%
  left_join(catastrophes_totales, by = c("Country", "Year")) %>%
  left_join(catastrophes_soustypes, by = c("Country", "Year")) %>%
  replace_na(list(Catastrophes_totales = 0))

# 7. NETTOYER ET RENOMMER

df_fusion <- df_fusion %>%
  rename(
    Pays = Country,
    Année = Year,
    Population = Population,  # ajuster selon noms exacts de colonnes si besoin
    Température = Celsius,
    CO2 = co2
  )

# 8. EXPORTER

write.xlsx(df_fusion, "Base_fusionnee_nouvelles_bases.xlsx")
write_csv(df_fusion, "Base_fusionnee_nouvelles_bases.csv")

cat("Export terminé : fichier Excel et CSV créés ✅\n")
