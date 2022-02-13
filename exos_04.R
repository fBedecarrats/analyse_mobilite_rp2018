library(readr)
library(readxl)
library(ggplot2)
library(dplyr)
library(forcats)
library(vroom)

# Chargement des données
# On utilise vroom qui est ~10x plus rapide pour lire de gros fichiers
flux <- vroom("data/FD_MOBPRO_2018.csv",
               col_types = cols_only(
                 COMMUNE = "character",
                 DCLT = "factor",
                 IPONDI = "numeric",
                 TRANS = "factor", 
                 ))



EPCI_FR <- read_excel("data/Intercommunalite_Metropole_au_01-01-2018.xls", 
                      sheet = "Composition_communale", skip = 5)

# Cette fonction liste les code communes correspondant à une intercommunalité
# Elle prend les arguments suivants en entrée :
# - Un dataframe ou tibble contenant les intercommunalités au format Insee
# - Une chaîne de caractères correspondant à une intercommunalité
communes_interco <- function(interco, registre_interco = EPCI_FR) {
  # TODO : inlure des tests
  registre_interco %>% 
    filter(LIBEPCI == interco) %>%
    select(CODGEO, LIBGEO)
}

communes_NM <- communes_interco("Nantes Métropole")

flux_nm <- flux %>%
  filter(COMMUNE %in% communes_NM$CODGEO | DCLT %in% communes_NM$CODGEO) %>%
  left_join(rename(communes_NM, `Commune de résidence` = LIBGEO), 
            by = c("COMMUNE" = "CODGEO")) %>%
  left_join(rename(communes_NM, `Commune de travail` = LIBGEO), 
            by = c("DCLT" = "CODGEO")) %>%
  mutate(`Mode de transport` =  fct_recode(TRANS,
                                           "Aucun" = "1",
                                           "Marche" = "2",
                                           "Vélo" = "3",
                                           "Moto" = "4",
                                           "Voiture" = "5",
                                           "TC" = "6"))

# On crée une séquence de noms de communes ordonnée par nombre de flux
# qu'on intitule : ordre_communes
ordre_communes <- flux_nm %>%
  group_by(`Commune de résidence`) %>%
  summarize(flux = sum(IPONDI, na.rm = TRUE)) %>%
  arrange(desc(flux)) %>%
  filter(!is.na(`Commune de résidence`)) %>%
  pull(`Commune de résidence`)

# On crée un facteur (càd liste de valeurs ordonnées) en classant par la 
# séquence ordre_communes
flux_nm <- flux_nm %>%
  mutate(`Commune de résidence` = factor(`Commune de résidence`,
                                         levels = ordre_communes),
         `Commune de travail` = factor(`Commune de travail`,
                                         levels = ordre_communes))

# Un premier graph qui compte le nombre de trajet par commune de résidence
flux_nm %>%
  ggplot(aes(x = `Commune de résidence`)) +
  geom_bar() 

# Les noms de communes se recoupent : on va les réorienter
flux_nm %>%
  ggplot(aes(x = `Commune de résidence`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Les données de recensement sont un échantillon : on voyait le nombre de
# personnes interrogées. On veut tenir compte de la pondération (IPONDI) 
# pour savoir à combien de trajets cela correspond en population totale
flux_nm %>%
  ggplot(aes(x = `Commune de résidence`, weight = IPONDI)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::label_number()) # pour avoir des nombres
# A noter, l'emploi de '::' avant une fonction. Cela veut dire qu'on dit à R
# d'utiliser la fonction label_number du package scales sans charger tout le 
# package en question avec 'library(scales)'

# Afficher les parts modales
flux_nm %>%
  ggplot(aes(x = `Commune de résidence`, weight = IPONDI,
             fill = `Mode de transport`)) + # A ajouter pour les modes de transport
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::label_number())

# Rétablir sur 100%
flux_nm %>%
  ggplot(aes(x = `Commune de résidence`, weight = IPONDI,
             fill = `Mode de transport`)) + # A ajouter pour les modes de transport
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::label_number())
# Exercice : mettre l'échelle de l'axe y en pourcentages


# On représente la matrice origine destination
flux_nm %>%
  ggplot(aes(x = "", fill = `Mode de transport`,
             weight = IPONDI)) + # On pondère à partir de l'échantillonage
  geom_bar(position = "fill") + # Pour afficher les résultats en proportion
  facet_grid(`Commune de résidence` ~ `Commune de travail`, # affichage en carreaux
             switch = "y") + # labels en ligne à gauche plutôt qu'à droite
  theme(axis.text = element_blank(), # pas de texte d'échelle 
        axis.ticks = element_blank(), # pas de tirets de repères
        strip.text.y.left = element_text(angle = 0), # orientation des noms de communes en ligne 
        strip.text.x = element_text(angle = 90), # orientation des noms de communes en colonnes
        panel.spacing = unit(0.2, "lines"),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))
# Exercice : ajouter un titre aux axes x et y.

# On va requêter l'API des découpages communaux 
# https://geo.api.gouv.fr/decoupage-administratif/communes
library(sf) # gère les objets spatiaux, cf. https://geocompr.robinlovelace.net
library(purrr) # pour vectoriser les appels avec map() : mieux que les boucles 'for' 

# On crée une fonction qui prend en entrée une liste de numéros de communes et 
# un type de géographie et qui renvoie le geojson correspondant
# cf. https://api.gouv.fr/documentation/api-geo
get_communes <- function(x, geo = c("centre", "contour")) {
  paste0("https://geo.api.gouv.fr/communes/",x,"?fields=", geo) %>% # forme la requête
    read_sf()
}

# On va ensuite pouvoir vectoriser cette option, càd l'appeler de manière répétée
# sur un vecteur
od_map <- map_df(communes_NM$CODGEO, get_communes, "contour") %>%
  bind_cols(communes_NM) %>%
  st_as_sf()

flux_domicile <- flux_nm %>%
  group_by(`Commune de résidence`) %>%
  summarise(domicile = sum(IPONDI, na.rm = TRUE)) # pour avoir les valeurs en population

flux_travail <- flux_nm %>%
  group_by(`Commune de travail`) %>%
  summarise(travail = sum(IPONDI, na.rm = TRUE)) # pour avoir les valeurs en population

od_map <- od_map %>%
  left_join(flux_domicile, by = c("LIBGEO" = "Commune de résidence")) %>%
  left_join(flux_travail, by = c("LIBGEO" = "Commune de travail"))

# On va maintenant pouvoir réaliser des cartes simples
library(tmap) # un package pour faire des cartes thématiques assez simples
# Un premier exemple vide
qtm(od_map$geometry)

qtm(od_map, fill="travail", fill.palette="viridis")

tm_shape(od_map) + 
  tm_fill(c("domicile", "travail"),  
          palette = viridis::plasma(5),
          breaks = c(0, 2000, 5000, 10000, 50000, 200000),
          title = "Flux")  +
  tm_borders(col = "black", lwd = 0.5) + 
  tm_facets(free.scales = FALSE, ncol = 2) +
  tm_layout(panel.labels = c("Domicile", "Travail"))




