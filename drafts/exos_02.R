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
                 EMPL = "factor",
                 NPERR = "factor",
                 AGEREVQ = "integer",
                 CS1 = "factor",
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

test <- flux_nm %>%
  group_by(`Mode de transport`, `Commune de résidence`, `Commune de travail`) %>%
  summarise(`Flux domicile-travail` = mean(IPONDI, na.rm = TRUE))

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
  