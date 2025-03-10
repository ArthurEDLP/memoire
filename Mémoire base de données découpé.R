library(readxl)
castrophes_naturelles <- read_excel("C:/Users/arthu/Downloads/castrophes naturelles.xlsx")

df <- data.frame(castrophes_naturelles)

Subgroup <- unique(df$Disaster.Subgroup)
print(Subgroup)

# Biological ----

Biological_TRUE <- grepl("Biological", df$Disaster.Subgroup)

Biological <- df[Biological_TRUE, ]

Type <- unique(Biological$Disaster.Type)
print(Type_Bio)

## Biological_Epidemic ----

Epidemic_TRUE <- grepl("Epidemic", Biological$Disaster.Type)

Biological_Epidemic <- Biological[Epidemic_TRUE, ]

## Biological_Infestation ----

Infestation_TRUE <- grepl("Infestation", Biological$Disaster.Type)

Biological_Infestation <- Biological[Infestation_TRUE, ]

## Biological_Animal ----

Animal_TRUE <- grepl("Animal", Biological$Disaster.Type)

Biological_Animal <- Biological[Animal_TRUE, ]


# Geophysical ----

Geophysical_TRUE <- grepl("Geophysical", df$Disaster.Subgroup)

Geophysical <- df[Geophysical_TRUE, ]

Type_Geo <- unique(Geophysical$Disaster.Type)
print(Type_Geo)

## Geophysical_Volcanic  ----

Volcanic_TRUE <- grepl("Volcanic ", Geophysical$Disaster.Type)

Geophysical_Volcanic <- Geophysical[Volcanic_TRUE, ]

## Geophysical_Earthquake  ----

Earthquake_TRUE <- grepl("Earthquake", Geophysical$Disaster.Type)

Geophysical_Earthquake <- Geophysical[Earthquake_TRUE, ]

## Geophysical_Mass   ----

Mass_TRUE <- grepl("Mass", Geophysical$Disaster.Type)

Geophysical_Mass <- Geophysical[Mass_TRUE, ]


# Climatological ----

Climatological_TRUE <- grepl("Climatological", df$Disaster.Subgroup)

Climatological <- df[Climatological_TRUE, ]

Type_Cli <- unique(Climatological$Disaster.Type)
print(Type_Cli)

## Climatological_Wildfire   ----

Wildfire_TRUE <- grepl("Wildfire", Climatological$Disaster.Type)

Climatological_Wildfire <- Climatological[Wildfire_TRUE, ]

## Climatological_Drought   ----

Drought_TRUE <- grepl("Drought", Climatological$Disaster.Type)

Climatological_Drought <- Climatological[Drought_TRUE, ]

## Climatological_Glacial   ----

Glacial_TRUE <- grepl("Glacial", Climatological$Disaster.Type)

Climatological_Glacial <- Climatological[Glacial_TRUE, ]


# Extra-terrestrial ----

Extra_terrestrial_TRUE <- grepl("Extra-terrestrial", df$Disaster.Subgroup)

Extra_terrestrial <- df[Extra_terrestrial_TRUE, ]

Type_Extra <- unique(Extra_terrestrial$Disaster.Type)
print(Type_Extra)


# Hydrological ----

Hydrological_TRUE <- grepl("Hydrological", df$Disaster.Subgroup)

Hydrological <- df[Hydrological_TRUE, ]

Type_Hydro <- unique(Hydrological$Disaster.Type)
print(Type_Hydro)

## Hydrological_Flood   ----

Flood_TRUE <- grepl("Flood", Hydrological$Disaster.Type)

Hydrological_Flood <- Hydrological[Flood_TRUE, ]

## Hydrological_Mass   ----

Mass_TRUE <- grepl("Mass", Hydrological$Disaster.Type)

Hydrological_Mass <- Hydrological[Mass_TRUE, ]


# Meteorological ----

Meteorological_TRUE <- grepl("Meteorological", df$Disaster.Subgroup)

Meteorological <- df[Meteorological_TRUE, ]

Type_Meteo <- unique(Meteorological$Disaster.Type)
print(Type_Meteo)

## Meteorological_Extreme   ----

Extreme_TRUE <- grepl("Extreme", Meteorological$Disaster.Type)

Meteorological_Extreme <- Meteorological[Extreme_TRUE, ]

## Meteorological_Storm   ----

Storm_TRUE <- grepl("Storm", Meteorological$Disaster.Type)

Meteorological_Storm <- Meteorological[Storm_TRUE, ]












