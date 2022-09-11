library(tictoc)
# On charge nos librairies
library(shiny) # Pour faire tourner l'application shiny
# reactiveConsole(enabled = TRUE)
library(readxl) # pour lire les fichiers excel (liste communes/epci)
library(vroom) # pour lire les csv (très rapide pour les 'gros' csv)
library(dplyr) # pour les manipulation de données
library(tidyr)
library(ggplot2) # pour les graphs
library(plotly) # pour des graphs interactifs
library(ggiraph)
library(forcats) # pour modifier des facteurs (variables catégorielles ordonnées)
library(sf) # gère les objets spatiaux, cf. https://geocompr.robinlovelace.net
library(stplanr)
library(tmap) # pour les cartes
library(purrr)
library(arrow)

EPCI_FR <- read_excel("data/Recensement/Intercommunalite_Metropole_au_01-01-2018.xls", 
                      sheet = "Composition_communale", skip = 5) %>%
  select(LIBEPCI, CODGEO, LIBGEO)



# write_parquet(flux, "data/Recensement/FD_MOBPRO_2018.parquet")


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

# On récupère l'EPCI sélectionnée dans l'UI
# my_epci <- reactive({ input$epci })
my_epci <- "Nantes Métropole"
# On liste les communes à garder
communes <- reactive({ communes_interco(interco = "Nantes Métropole") })


# Test sans arrow ---------------------------------------------------------

tic()

flux <- vroom("data/Recensement/FD_MOBPRO_2018.csv",
              col_types = cols_only(
                COMMUNE = "character",
                DCLT = "character",
                IPONDI = "numeric",
                TRANS = "factor")) %>%
  mutate(`Mode de transport` =  fct_recode(factor(TRANS),
                                           "Aucun" = "1",
                                           "Marche" = "2",
                                           "Vélo" = "3",
                                           "Moto" = "4",
                                           "Voiture" = "5",
                                           "TC" = "6"))

# On crée une variable réactive avec la matrice origine-destination sélectionnée
flux_epci <- reactive({
  my_communes <- communes()
  # On filtre la donnée de flux pour ne garder que ces communes
  flux_epci <- flux %>%
    filter(COMMUNE %in% my_communes$CODGEO | DCLT %in% my_communes$CODGEO) %>%
    # On crée des intitulés lisibles: nom de commune au lieu des numéros
    left_join(rename(my_communes, `Commune de résidence` = LIBGEO), 
              by = c("COMMUNE" = "CODGEO")) %>%
    left_join(rename(my_communes, `Commune de travail` = LIBGEO), 
              by = c("DCLT" = "CODGEO")) 
  
  # On affiche les commune par ordre décroissant de flux : ça nous servira
  # à ordonner l'affichage des communes sur le graph
  ordre_communes <- flux_epci %>%
    group_by(`Commune de résidence`) %>%
    summarize(flux = sum(IPONDI, na.rm = TRUE)) %>%
    arrange(desc(flux)) %>%
    filter(!is.na(`Commune de résidence`)) %>%
    pull(`Commune de résidence`)
  
  # L'ordre est passée en transformant le libellé des communes en une variable
  # catégorielle ordonnée ("factor" dans R, ce qu'on appelle "domaines" dans Argis)
  flux_epci <- flux_epci %>%
    mutate(`Commune de résidence` = factor(`Commune de résidence`,
                                           levels = ordre_communes),
           `Commune de travail` = factor(`Commune de travail`,
                                         levels = ordre_communes)) 
  return(flux_epci)
})

intermediaire_total <- reactive({
  flux_epci() %>% # on enlève le mode du group_by
    group_by(`Commune de résidence`, `Commune de travail`) %>%
    summarise(`Flux total par paire de communes` = sum(IPONDI, na.rm = TRUE))
})

matrice_od_epci <- reactive({
  # Cette étape est factultative mais fait gagner ~2secondes de traitement
  # car dplyr est un peu plus rapide que ggplot2
  intermediaire <- flux_epci() %>%
    group_by(`Mode de transport`, `Commune de résidence`, `Commune de travail`) %>%
    summarise(`Flux domicile-travail` = sum(IPONDI, na.rm = TRUE)) %>%
    left_join(intermediaire_total(), by = c("Commune de résidence", 
                                            "Commune de travail")) %>%
    mutate(part_mod = `Flux domicile-travail` / `Flux total par paire de communes`,
           txt = paste(`Mode de transport`, "\nPart modale :", 
                       round(part_mod * 100, 1), "%\nTrajets :", 
                       round(`Flux domicile-travail`), "/", 
                       round(`Flux total par paire de communes`)))
  return(intermediaire)
})

# test <- matrice_od_epci()

static_plot <- matrice_od_epci() %>% # marche aussi en partant de flux_epci, mais + long
  ggplot() +
  geom_bar_interactive(aes(x = "", fill = `Mode de transport`,
                           tooltip = txt, data_id = txt), position = "fill",
                       size = 1) + # Pour afficher les résultats en proportion
  facet_grid(`Commune de résidence` ~ `Commune de travail`, # affichage en carreaux
             switch = "y") + # labels en ligne à gauche plutôt qu'à droite
  theme(axis.text = element_blank(), # pas de texte d'échelle
        axis.ticks = element_blank(), # pas de tirets de repères
        strip.text.y.left = element_text(angle = 0), # orientation des noms de communes en ligne
        strip.text.x = element_text(angle = 90), # orientation des noms de communes en colonnes
        panel.spacing = unit(0.2, "lines"),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "top") +
  guides(fill = guide_legend(nrow = 1))
static_plot



toc()
# 10.45 sec elapsed juste flux_epci
# 9.518 sec elapsed pout matrice_od_epci
# 21.903 sec elapsed avec les graphs

# Test avec arrow ---------------------------------------------------------

tic()
flux <- open_dataset("data/Recensement/FD_MOBPRO_2018.parquet")

flux_epci <- reactive({ 
  my_communes <- communes()
  # On filtre la donnée de flux pour ne garder que ces communes
  flux %>%
    filter(COMMUNE %in% my_communes$CODGEO | DCLT %in% my_communes$CODGEO) %>%
    # On crée des intitulés lisibles: nom de commune au lieu des numéros
    left_join(rename(my_communes, `Commune de résidence` = LIBGEO), 
              by = c("COMMUNE" = "CODGEO")) %>%
    left_join(rename(my_communes, `Commune de travail` = LIBGEO), 
              by = c("DCLT" = "CODGEO"))
  })

intermediaire_total <- reactive({
  flux_epci() %>% # on enlève le mode du group_by
    group_by(`Commune de résidence`, `Commune de travail`) %>%
    summarise(`Flux total par paire de communes` = sum(IPONDI, na.rm = TRUE))
})

matrice_od_epci <- reactive({
  # Cette étape est factultative mais fait gagner ~2secondes de traitement
  # car dplyr est un peu plus rapide que ggplot2
  intermediaire <- flux_epci() %>%
    group_by(`Mode de transport`, `Commune de résidence`, `Commune de travail`) %>%
    summarise(`Flux domicile-travail` = sum(IPONDI, na.rm = TRUE)) %>%
    left_join(intermediaire_total(), by = c("Commune de résidence", 
                                            "Commune de travail")) %>%
    mutate(part_mod = `Flux domicile-travail` / `Flux total par paire de communes`,
           txt = paste(`Mode de transport`, "\nPart modale :", 
                       round(part_mod * 100, 1), "%\nTrajets :", 
                       round(`Flux domicile-travail`), "/", 
                       round(`Flux total par paire de communes`))) %>%
  collect()
  return(intermediaire)
})


# On affiche les commune par ordre décroissant de flux : ça nous servira
# à ordonner l'affichage des communes sur le graph
ordre_communes <- reactive({
  matrice_od_epci() %>%
    group_by(`Commune de résidence`) %>%
    summarize(flux_tot = sum(`Flux domicile-travail`, na.rm = TRUE)) %>%
    arrange(desc(flux_tot)) %>%
    filter(!is.na(`Commune de résidence`)) %>%
    pull(`Commune de résidence`)
})


# L'ordre est passée en transformant le libellé des communes en une variable
# catégorielle ordonnée ("factor" dans R, ce qu'on appelle "domaines" dans Argis)
flux_epci_ordered <- flux_epci() %>%
  mutate(`Commune de résidence` = factor(`Commune de résidence`,
                                         levels = ordre_communes()),
         `Commune de travail` = factor(`Commune de travail`,
                                       levels = ordre_communes())) 


# test <- matrice_od_epci()

static_plot <- matrice_od_epci() %>% # marche aussi en partant de flux_epci, mais + long
  mutate(`Commune de résidence` = factor(`Commune de résidence`,
                                         levels = ordre_communes()),
         `Commune de travail` = factor(`Commune de travail`,
                                       levels = ordre_communes())) %>%
  ggplot() +
  geom_bar_interactive(aes(x = "", fill = `Mode de transport`,
                           tooltip = txt, data_id = txt), position = "fill",
                       size = 1) + # Pour afficher les résultats en proportion
  facet_grid(`Commune de résidence` ~ `Commune de travail`, # affichage en carreaux
             switch = "y") + # labels en ligne à gauche plutôt qu'à droite
  theme(axis.text = element_blank(), # pas de texte d'échelle
        axis.ticks = element_blank(), # pas de tirets de repères
        strip.text.y.left = element_text(angle = 0), # orientation des noms de communes en ligne
        strip.text.x = element_text(angle = 90), # orientation des noms de communes en colonnes
        panel.spacing = unit(0.2, "lines"),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "top") +
  guides(fill = guide_legend(nrow = 1))
static_plot


toc()
# 0.925 sec elapsed pour flux_epci
# 1.814 sec elapsed pour matrice_od_epci
# 16.805 sec elapsed avec le graph