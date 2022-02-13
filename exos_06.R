
# Chargement des librairies -----------------------------------------------

library(vroom) # pour lire les csv (très rapide pour les 'gros' csv)
library(readxl) # pour lire les fichiers excel (liste communes/epci)
library(dplyr) # pour les manipulation de données
library(tidyr) # pour les pivots
library(forcats) # pour modifier des facteurs (variables catégorielles ordonnées)
library(purrr) # pour vectoriser les appels avec map() : mieux que les boucles 'for'
library(ggplot2) # pour les graphs
library(plotly) # pour des graphs interactifs
library(sf) # gère les objets spatiaux, cf. https://geocompr.robinlovelace.net
library(stplanr)
library(tmap) # pour les cartes

# Chargement des données --------------------------------------------------

# On utilise vroom qui est ~10x plus rapide pour lire de gros fichiers
flux <- vroom("data/FD_MOBPRO_2018.csv",
               col_types = cols_only( # on ne charge que certaines colonnes
                 COMMUNE = "character", # commune de résidence
                 DCLT = "factor", # commune de travail
                 IPONDI = "numeric", # pondération
                 TRANS = "factor")) # mode de transport

# On reprend la base des communes/epci pour l'année correspondante
EPCI_FR <- read_excel("data/Intercommunalite_Metropole_au_01-01-2018.xls", 
                      sheet = "Composition_communale", skip = 5)

# Filtre sur l'EPCI -------------------------------------------------------

# Cette fonction liste les code communes correspondant à une intercommunalité
# Elle prend les arguments suivants en entrée :
# - Un dataframe ou tibble contenant les intercommunalités au format Insee
# - Une chaîne de caractères correspondant à une intercommunalité
communes_interco <- function(interco, registre_interco = EPCI_FR) {
  # TODO : inlure des tests
  registre_interco %>% 
    filter(LIBEPCI == interco) %>%
    select(LIBGEO, CODGEO)
}

# On extrait la liste de communes de Nantes Métropole
communes_NM <- communes_interco("Nantes Métropole")

# On sélectionne les flux domicile travail qui entrent ou sortent de l'EPCI
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


# Premier graphique simple ---------------------------------------------

# Petite explication séquentielle en 2 étapes, répétées de 2 manières différentes

# Etape 1 : premier graph avec le nombre de trajet par commune de résidence
flux_nm %>%
  ggplot(aes(x = `Commune de résidence`)) +
  geom_bar() 
# Premier problème : les intitulés ne sont pas lisibles on reprend

# Etape 2 : on réoriente les noms des communes
flux_nm %>%
  ggplot(aes(x = `Commune de résidence`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# A noter : les graphes sont composées de couches programmatiques. On peut ainsi
# Stocker le graphe dans une variable
mon_graph <- flux_nm %>% # Etape 1, mais stockée dans une variable
  ggplot(aes(x = `Commune de résidence`)) +
  geom_bar() 
# Si j'appelle la variable, le graph s'affiche
mon_graph
# Mais je peux venir rajouter des paramètrs au graph
mon_graph <- mon_graph + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Si je l'imprime, il a maintenant repris la modification
mon_graph


# Travail sur l'ordre des valeurs ---------------------------------------

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

# Le même graphique, mais avec un ordre décroissant (et les hors EPCI à la fin)
flux_nm %>%
  ggplot(aes(x = `Commune de résidence`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Prise en compte de l'échantillonnage ------------------------------------

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

# Exercice : télécharger les flux totaux calculés par l'INSEE pour chaque paire de communes
# https://www.insee.fr/fr/statistiques/fichier/5393835/base-csv-flux-mobilite-domicile-lieu-travail-2018.zip
# et vérifier que vous obtenez le même chiffre qu'avec les variables de pondération
# NB ; lINSEE ne calcule pas le détail par part modale, juste les flux totaux entre 2 communes.

# Construction d'une vue plus complexe ------------------------------------

# Afficher les parts modales
flux_nm %>%
  ggplot(aes(x = `Commune de résidence`, weight = IPONDI,
             fill = `Mode de transport`)) + # A ajouter pour les modes de transport
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::label_number())

# Rétablir sur 100%
graph_mod <- flux_nm %>%
  ggplot(aes(x = `Commune de résidence`, weight = IPONDI,
             fill = `Mode de transport`)) + # A ajouter pour les modes de transport
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank()) +
  scale_y_continuous(labels = scales::label_percent())
# Exercice : mettre l'échelle de l'axe y en pourcentages
# Exercice : produire le même graphique avec la commune de travail


# On représente la matrice origine destination. NB : Tout se passe sur les 4 
# premières lignes (facet), la suite n'est que de la mise en forme
graph_od_mod <- flux_nm %>%
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
# On l'affiche
graph_od_mod
# Exercice : ajouter un titre aux axes x et y.

# Graphique interactif ----------------------------------------------------

# Il existe différentes manières de créer des graphs interactifs dans R
# on va ici utiliser plotly, une librairie javascript très populaire

# On reproduit le graphique ggplot 'graph_mod' créé plus haut, en le passant
# simplement à plotly avec la fonction 'ggplotly'
ggplotly(graph_mod, tooltip = c("x", "fill", "count"))
# L'argument "tooltip" permet de sélectionner les infos à afficher au passage
# de la souris sur le graph. Malheureusement, pour afficher un pourcentage 
# correct, on doit faire des calculs préalables afin que les valeurs en entrée
# soient déjà calculés

# On recalcule explicitement les parts modales par commune que ggplot calculait
# automatiquement afin de pouvoir les passer en entrée
perc_flux <- flux_nm %>% 
  count(`Commune de résidence`, `Mode de transport`, wt = IPONDI) %>%
  right_join(count(flux_nm, `Commune de résidence`, wt = IPONDI), 
             by = "Commune de résidence") %>%
  mutate(part_mod = paste("Part modale :", round(n.x / n.y * 100, 1), "%",
                          "\nTrajets :", round(n.x), "/", round(n.y)))

# On passe ces valeurs en entrée à ggplot à une variable "text" pas utilisée
# par ggplot mais qui pourra être incluse dans l'objet de sortie et récupérée
# par plotly
graph_mod <- perc_flux %>%
  ggplot(aes(x = `Commune de résidence`, weight = n.x,
             fill = `Mode de transport`, 
             text = part_mod)) + # A ajouter pour les modes de transport
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank()) +
  scale_y_continuous(labels = scales::label_percent())

# la variable "text" qui contient le pourcentage "bien formé" peut désormais 
# être récupérée et affichée par plotly
ggplotly(graph_mod, tooltip = c("x", "fill", "text"))

# Exercice (très difficile, à garder pour la fin peut-être), passer le graphique
# de matrice origine-destination en interactif.

# Création de cartes ------------------------------------------------------

# Ce qui suit est largement repris de : https://geocompr.robinlovelace.net/transport.html

# On va requêter l'API des découpages communaux 
# https://geo.api.gouv.fr/decoupage-administratif/communes

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

# Un premier exemple vide
qtm(mobpro_choro_s)


# Choroplèthes --------------------------------------------------------------
# Choroplèthe : est une carte thématique où les aires sont colorées 
# pour caractériser statistiquement un phénomène

# on calcule les flux par paires de communes pour les afficher sur la carte

flux_domicile <- flux_nm %>% # somme des flux par commune de domicile
  group_by(`Commune de résidence`) %>%
  summarise(domicile = sum(IPONDI, na.rm = TRUE)) # pour avoir les valeurs en population

flux_travail <- flux_nm %>% # somme des flux par commune de travail
  group_by(`Commune de travail`) %>%
  summarise(travail = sum(IPONDI, na.rm = TRUE)) # pour avoir les valeurs en population

mobpro_choro_s <- mobpro_choro_s %>% # on les joint à la couche géographique
  left_join(flux_domicile, by = c("LIBGEO" = "Commune de résidence")) %>%
  left_join(flux_travail, by = c("LIBGEO" = "Commune de travail"))

# Ensuite on choisit la variable qui servira de base au remplissage (ici 'travail')
qtm(mobpro_choro_s, fill="travail", legend.outside = TRUE)
# On voit qu'on a besoin de retravailer les seuils

# On teste une série d'algos automatiques pour la coloration
qtm(mobpro_choro_s, fill="travail", fill.style = "cont", legend.outside = TRUE)
qtm(mobpro_choro_s, fill="travail", fill.style = "order", legend.outside = TRUE)
qtm(mobpro_choro_s, fill="travail", fill.style = "jenks", legend.outside = TRUE)
qtm(mobpro_choro_s, fill="travail", fill.style = "fisher", legend.outside = TRUE)
# plus de détails sur https://geocompr.github.io/post/2019/tmap-color-scales/

# On prépare un modèle à la main
tm_shape(mobpro_choro_s) + 
  tm_fill(c("domicile", "travail"),
          #style = "fisher", # Pour
          breaks = c(100, 7500, 15000, 30000, 60000, 120000, 180000, 200000), # On arrondi un peu les seuils
          text.separator = "-",
          title = "Flux")  +
  tm_borders(col = "black", lwd = 0.5) + 
  tm_facets(free.scales = FALSE, ncol = 2) +
  tm_layout(panel.labels = c("Domicile", "Travail"),
            legend.format=list(fun=function(x) formatC(x, digits=0, format="d"),
                               text.separator = "-"))

# Carte de flux -----------------------------------------------------------

# On récupère le centre des communes 
centres_communes_p <- map_df(communes_NM$CODGEO, get_communes, "centre") %>%
  bind_cols(select(communes_NM, LIBGEO)) %>% # on leur rattache leur nom
  # la fonction od2line requiert que le champ identifiant soit le premier à gauche
  relocate(LIBGEO, .before = geometry) 

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

# On exclut les communes extérieures à l'EPCI (pas de géométrie)
# et les flux où on a la même commune en domicile-travail.
mobpro_od <- mobpro_od %>%
  filter(!is.na(`Commune de résidence`) & !is.na(`Commune de travail`)) %>%
  filter(`Commune de résidence` != `Commune de travail`) %>%
  # On passe les noms de communes de facteur à caractère pour permettre la jointure
  mutate(`Commune de résidence` = as.character(`Commune de résidence`),
         `Commune de travail` = as.character(`Commune de travail`))

# On utilise la fonction du od2line du package stplanr
# A lire : https://docs.ropensci.org/stplanr/
mobpro_od_2l <- od2line(flow = mobpro_od, zones = centres_communes_p)

# Cette matrice est bidirectionnelles : le flux Nantes-Rezé se supperpose au 
# flux Rezé-Nantes. On va cumuler les flux en utilisant la fonction od_oneway
# puis calculer des agrégats en rassemblant les modes de transports d'intérêt
mobpro_od_1l <- mobpro_od_2l %>%
  od_oneway() %>%
  mutate(`Modes actifs (sans TC)` = rowSums(cbind(`Vélo`, Marche), na.rm = TRUE) / total,
         `Modes durables (avec TC)` = rowSums(cbind(`Vélo`, Marche, TC), na.rm = TRUE) / total)

# On représente le résultat
tm_shape(mobpro_od_1l) +
  tm_lines(palette = viridis::plasma(10),
           lwd = "total",
           breaks = c(0:5)/10,
           scale = 9,
           title.lwd = "Nombre de trajets",
           alpha = 1,
           col = c("Modes durables (avec TC)", "Modes actifs (sans TC)"),
           text.separator = "-",
           title = "Part modale") +
  tm_layout(panel.labels = c("Modes durables (avec TC)", "Modes actifs (sans TC)"),
            legend.format=list(fun=function(x) ifelse(x > 1, x,
                                                      paste0(formatC(x*100, digits=0, format="f"), " %")),
            text.separator = "-"),
            legend.outside = TRUE)

# Ces cartes sont plus jolies à regarder avec un fond, en mode "view"
tmap_mode("view")
tm_shape(mobpro_od_1l) +
  tm_lines(palette = viridis::plasma(10),
           lwd = "total",
           breaks = c(0:5)/10,
           scale = 9,
           title.lwd = "Nombre de trajets",
           alpha = 1,
           col ="Modes durables (avec TC)", 
           text.separator = "-",
           title = "Part modale") +
  tm_layout(legend.format=list(fun=function(x) ifelse(x > 1, x,
                                                      paste0(formatC(x*100, digits=0, format="f"), " %")),
                               text.separator = "-"),
            legend.outside = TRUE)
# Exercice : modifier les fonds de carte avec un fond OSM ou ESRI, 
# cf : https://r-tmap.github.io/tmap-book/layers.html#raster-layer  
