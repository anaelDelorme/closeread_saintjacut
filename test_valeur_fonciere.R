# Chargement des packages nécessaires
library(tidyverse)
library(lubridate)
library(scales)

# Lecture et préparation des données
transactions_brutes <- read_csv("data/dvf.csv") 
transactions <- transactions_brutes %>% 
  mutate(
    date_mutation = ymd(date_mutation),
    # Création d'une variable type_bien plus générale
    type_bien = case_when(
      !is.na(type_local) ~ type_local,
      nature_culture == "terrains a bâtir" ~ "Terrain à bâtir",
     is.na(type_local) ~ "Terrain",
      TRUE ~ "Autre"
    ),
    # Calcul du prix au m² (différent selon le type de bien)
    prix_m2 = case_when(
      type_bien %in% c("Maison", "Appartement", "Dépendance") ~ valeur_fonciere / surface_reelle_bati,
      type_bien %in% c("Terrain à bâtir", "Terrain") ~ valeur_fonciere / surface_terrain,
      TRUE ~ NA_real_
    )
  ) 

# 1. Analyse de Saint-Jacut par type de bien
analyse_saint_jacut_types <- transactions %>%
  filter(nom_commune == "Saint-Jacut-de-la-Mer") %>%
  group_by(type_bien) %>%
  summarise(
    nombre_ventes = n(),
    prix_m2_moyen = mean(prix_m2, na.rm = TRUE),
    prix_m2_median = median(prix_m2, na.rm = TRUE),
    prix_moyen = mean(valeur_fonciere, na.rm = TRUE),
    surface_moyenne = mean(case_when(
      type_bien %in% c("Maison", "Appartement", "Dépendance") ~ surface_reelle_bati,
      TRUE ~ surface_terrain
    ), na.rm = TRUE)
  )

# 2. Comparaison avec les autres communes par type de bien
comparaison_communes_types <- transactions %>%
  group_by(nom_commune, type_bien) %>%
  summarise(
    nombre_ventes = n(),
    prix_m2_moyen = mean(prix_m2, na.rm = TRUE),
    prix_m2_median = median(prix_m2, na.rm = TRUE)
  ) %>%
  filter(nombre_ventes >= 3) %>% # Pour avoir des moyennes significatives
  arrange(type_bien, desc(prix_m2_moyen))

# 3. Visualisation des prix par type de bien
ggplot_prix_types <- ggplot(
  filter(transactions, nom_commune == "Saint-Jacut-de-la-Mer"),
  aes(x = type_bien, y = prix_m2, fill = type_bien)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution des prix au m² par type de bien à Saint-Jacut-de-la-Mer",
       x = "Type de bien",
       y = "Prix au m² (€)") +
  scale_y_continuous(labels = label_number(suffix = " €"))

# 4. Évolution temporelle par type de bien
evolution_types <- transactions %>%
  filter(nom_commune == "Saint-Jacut-de-la-Mer") %>%
  group_by(month = floor_date(date_mutation, "month"), type_bien) %>%
  summarise(
    nombre_ventes = n(),
    prix_m2_moyen = mean(prix_m2, na.rm = TRUE)
  )

# 5. Comparaison de la répartition des types de biens
repartition_types <- transactions %>%
  group_by(nom_commune) %>%
  mutate(total_commune = n()) %>%
  group_by(nom_commune, type_bien) %>%
  summarise(
    proportion = n() / first(total_commune) * 100,
    nombre = n()
  ) %>%
  filter(nombre >= 3) %>%
  arrange(nom_commune, desc(proportion))

# 6. Visualisation de la répartition des types de biens
ggplot_repartition <- ggplot(repartition_types,
                             aes(x = nom_commune, y = proportion, fill = type_bien)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Répartition des types de biens par commune",
       x = "Commune",
       y = "Proportion (%)")

# 7. Analyse des caractéristiques spécifiques par type
caracteristiques_types <- transactions %>%
  filter(nom_commune == "Saint-Jacut-de-la-Mer") %>%
  group_by(type_bien) %>%
  summarise(
    surface_moyenne = mean(case_when(
      type_bien %in% c("Maison", "Appartement") ~ surface_reelle_bati,
      TRUE ~ surface_terrain
    ), na.rm = TRUE),
    nombre_pieces_moyen = mean(nombre_pieces_principales, na.rm = TRUE),
    prix_total_moyen = mean(valeur_fonciere, na.rm = TRUE)
  )

# 8. Comparaison des ratios prix/surface avec les communes voisines
comparaison_voisinage <- transactions %>%
  group_by(nom_commune, type_bien) %>%
  summarise(
    prix_m2_moyen = mean(prix_m2, na.rm = TRUE),
    nombre_transactions = n()
  ) %>%
  filter(nombre_transactions >= 3) %>%
  pivot_wider(
    names_from = type_bien,
    values_from = prix_m2_moyen,
    values_fill = NA
  )
