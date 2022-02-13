library(vroom) # pour lire les csv (très rapide pour les 'gros' csv)
library(readxl) # pour lire les fichiers excel (liste communes/epci)
library(dplyr) # pour les manipulation de données
library(tidyr) # pour les pivots
library(forcats) # pour modifier des facteurs (variables catégorielles ordonnées)
library(ggplot2) # pour les graphs
library(tmap) # pour les cartes

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
mobpro_choro_s <- map_df(communes_NM$CODGEO, get_communes, "contour") %>%
  bind_cols(communes_NM) %>%
  st_as_sf()

flux_domicile <- flux_nm %>%
  group_by(`Commune de résidence`) %>%
  summarise(domicile = sum(IPONDI, na.rm = TRUE)) # pour avoir les valeurs en population

flux_travail <- flux_nm %>%
  group_by(`Commune de travail`) %>%
  summarise(travail = sum(IPONDI, na.rm = TRUE)) # pour avoir les valeurs en population

mobpro_choro_s <- mobpro_choro_s %>%
  left_join(flux_domicile, by = c("LIBGEO" = "Commune de résidence")) %>%
  left_join(flux_travail, by = c("LIBGEO" = "Commune de travail"))

# On va maintenant pouvoir réaliser des cartes simples
library(tmap) # un package pour faire des cartes thématiques assez simples
# Un premier exemple vide
qtm(mobpro_choro_s$geometry)

# Ensuite on choisit la couleur de remplissage
qtm(mobpro_choro_s, fill="travail", fill.palette="viridis")
# On voit qu'on a besoin de spécifier les seuils de couleur

qtm(mobpro_choro_s, fill="travail", fill.style="pretty")

tm_shape(mobpro_choro_s) + 
  tm_fill(c("domicile", "travail"),
          #style = "fisher", # Pour une première approx avec des seuils pas ronds
          breaks = c(100, 7500, 15000, 30000, 60000, 120000, 160000), # On arrondi un peu les seuils
          title = "Flux")  +
  tm_borders(col = "black", lwd = 0.5) + 
  tm_facets(free.scales = FALSE, ncol = 2) +
  tm_layout(panel.labels = c("Domicile", "Travail"))

# On récupère le centre des communes 
centres_communes_p <- mobpro_choro_s <- map_df(communes_NM$CODGEO, get_communes, "centre") %>%
  bind_cols(select(communes_NM, LIBGEO)) # on leur rattache leur nom

# On calcule les flux entre chaque paire de communes
# d'abord avec 1 ligne par type de mode de transport (vélo, voiture...)
mobpro_od <- flux_nm %>%
  group_by(`Commune de résidence`, `Commune de travail`, `Mode de transport`) %>%
  summarise(trajets = sum(IPONDI, na.rm = TRUE)) %>%
  # on passe les modes en colonnes pour n'avoir plus qu'1 ligne par paire de communes
  pivot_wider(names_from = `Mode de transport`, values_from = trajets)

# On calcule les totaux (on pourrait le faire à partir des modes, 
# mais ça marche aussi comme ça)
mobpro_od <- flux_nm %>%
  group_by(`Commune de résidence`, `Commune de travail`) %>% # sans mode de transport
  summarise(total = sum(IPONDI, na.rm = TRUE)) %>%
  right_join(mobpro_od, by = c("Commune de résidence", "Commune de travail"))

# On associe les valeurs avec la localisation des centres des communes
mobpro_od_p <- mobpro_od %>%
  left_join(rename(centre_communes_p, # on vient mettre la géométrie correspondant à la commune de résidence
                   geo_residence = geometry, # on renomme le champ géométrie pour savoir de laquelle il s'agit
                   `Commune de résidence` = LIBGEO), # on harmonise le nom de commune pour simplifier la jointure
            by = "Commune de résidence") %>% # on joint sur le nom de commune qui a maintenant le même nom
  left_join(rename(centre_communes_p, # on vient mettre la géométrie correspondant à la commune de résidence
                   geo_travail = geometry, # on renomme le champ géométrie pour savoir de laquelle il s'agit
                   `Commune de travail` = LIBGEO), # on harmonise le nom de commune pour simplifier la jointure
            by = "Commune de travail") %>% # on vient mettre la géométrie correspondant à la commune de résidence
  relocate(starts_with("geo"), .before = 1) # on met les communes de géographie en début de table, requis pour 

# On transforme les paires de points origine-destination en lignes
mobpro_od_l <- od2line(mobpro_od_p, zones_od)

library(sf)
class(mobpro_od_p)
# On rattache les valeurs
centres_communes_p2 <- select(centres_communes_p, LIBGEO, geometry)

mobpro_od2 <- mobpro_od %>%
  filter(!is.na(`Commune de résidence`) & !is.na(`Commune de travail`)) %>%
  mutate(`Commune de résidence` = as.character(`Commune de résidence`),
         `Commune de travail` = as.character(`Commune de travail`))

test <- od2line(flow = mobpro_od2, zones = centres_communes_p2)


