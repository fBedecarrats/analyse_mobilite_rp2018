# library(shiny)
# reactiveConsole(enabled = TRUE) # A lancer pour exécuter dans la console

# On liste les librairies dont on a besoin
  library(shiny) # Pour faire tourner l'application shiny
  library(readxl) # pour lire les fichiers excel (liste communes/epci)
  library(vroom) # pour lire les csv (très rapide pour les 'gros' csv)
  library(dplyr) # pour les manipulation de données
  library(tidyr) # Pour les pivots
  library(ggplot2) # pour les graphs
  library(plotly) # pour des graphs interactifs
  library(ggiraph) # Pour des graphs interactifs
  library(forcats) # pour modifier des facteurs (variables catégorielles ordonnées)
  library(sf) # gère les objets spatiaux, cf. https://geocompr.robinlovelace.net
  library(stplanr) # Pour les routes
  library(tmap) # pour les cartes
  library(purrr) # Pour des opérations vectorisées
  library(arrow) # Pour de la lecture performante de fichiers volumineur
  library(duckdb)# Pour gérer des fichiers plus volumineux que la mémoire (sur shinyapps)
  library(waiter) # Pour des animations d'attente


# librairies_requises <- c( # On liste les librairies dont on a besoin
#   "shiny", # Pour faire tourner l'application shiny
#   "readxl", # pour lire les fichiers excel (liste communes/epci)
#   "vroom", # pour lire les csv (très rapide pour les 'gros' csv)
#   "dplyr", # pour les manipulation de données
#   "tidyr", # Pour les pivots
#   "ggplot2", # pour les graphs
#   "plotly", # pour des graphs interactifs
#   "ggiraph", # Pour des graphs interactifs
#   "forcats", # pour modifier des facteurs (variables catégorielles ordonnées)
#   "sf", # gère les objets spatiaux, cf. https://geocompr.robinlovelace.net
#   "stplanr", # Pour les routes
#   "tmap", # pour les cartes
#   "purrr", # Pour des opérations vectorisées
#   "arrow", # Pour de la lecture performante de fichiers volumineur
#   "duckdb", # Pour gérer des fichiers plus volumineux que la mémoire (sur shinyapps)
#   "waiter") # Pour des animations d'attente
# 
# # On regarde parmi ces librairies lesquelles ne sont pas installées
# manquantes <- !(librairies_requises %in% installed.packages())
# # On installe celles qui manquent
# if(any(manquantes)) install.packages(librairies_requises[manquantes])
# # Il faut absolument avoir  arrow à jour
# update.packages(oldPkgs = c("arrow", "duckdb"), ask = FALSE)
# # On charge toutes les librairies requises
# invisible(lapply(librairies_requises, require, character.only= TRUE))



# Pour préparer le jeu de données en entrée
if (!file.exists("FD_MOBPRO_2016_2019.parquet")) {
  source("prep_files.R")
}


# On charge la liste des communes de 2018 (on pourrait en avoir plusieurs verisons)
EPCI_FR <- read_excel("Intercommunalite_Metropole_au_01-01-2019.xls", 
                      sheet = "Composition_communale", skip = 5) %>%
  select(LIBEPCI, CODGEO, LIBGEO)


ui <- fluidPage(
  autoWaiter(), # Pout avoir des menus d'attente
  fluidRow(
    column(3, titlePanel("Trajets domicile-travail")),
    # column(2, "Choisir une EPCI"),
    column(4, helpText(" "),
           selectInput("epci", "\nChoisir une EPCI", # label = NULL,
                       choices = sort(unique(EPCI_FR$LIBEPCI)),
                       selected = "Nantes Métropole")),
    # column(2, "Année d'analyse"),
    column(4, helpText(" "),
           selectInput("annee", "\nAnnée d'analyse", # label = NULL,
                          choices = 2019:2017,
                          selected = 2018))),
  fluidRow(
    column(12, tabsetPanel(
      tabPanel("Matrice origine/destination", 
               plotOutput("matrice_od_commune_mode", height = "800px")),
      tabPanel("Histogramme par origine", 
               plotlyOutput("histo", height = "500px")),
      tabPanel("Carte des flux origine/destination", 
               sidebarPanel(width = 2,
                 radioButtons("modes", "Modes pris en compte",
                              choices = c("Modes durables (avec TC)",
                                          "Modes actifs (sans TC)"),
                              selected = "Modes durables (avec TC)")),
               mainPanel(tmapOutput("map")))))
  )
)
    


server <- function(input, output) {
  # Chargement des données
  # Nouvelle version avec arrow, encore 10x plus rapide
  flux <- open_dataset("FD_MOBPRO_2016_2019.parquet",
                       partitioning = c("annee", "epci_resid", "epci_travail"))
  
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

  # # On liste les communes à garder
  communes <- reactive({ communes_interco(interco = input$epci) })
  
  # On crée une variable réactive avec la matrice origine-destination sélectionnée
  flux_epci <- reactive({ 
    my_communes <- communes()
    # On filtre la donnée de flux pour ne garder que ces communes
    flux %>%
      filter(annee == as.integer(input$annee) & epci_resid == input$epci &
             epci_travail == input$epci) %>%
      select(-annee, -epci_resid, -epci_travail) %>%
      to_duckdb()
      # # On crée des intitulés lisibles: nom de commune au lieu des numéros
      # left_join(rename(my_communes, `Commune de résidence` = LIBGEO), 
      #           by = c("COMMUNE" = "CODGEO")) %>%
      # left_join(rename(my_communes, `Commune de travail` = LIBGEO), 
      #           by = c("DCLT" = "CODGEO"))
  })

  intermediaire_total <- reactive({
    flux_epci() %>% # on enlève le mode du group_by
      group_by(`Commune de résidence`, `Commune de travail`) %>%
      summarise(`Flux total par paire de communes` = sum(IPONDI, na.rm = TRUE))
  })
  
  matrice_od_epci <- reactive({
    # Cette étape est factultative mais fait gagner ~2secondes de traitement
    # car dplyr est un peu plus rapide que ggplot2
   flux_epci() %>%
      group_by(`Mode de transport`, `Commune de résidence`, `Commune de travail`) %>%
      summarise(`Flux domicile-travail` = sum(IPONDI, na.rm = TRUE)) %>%
      left_join(intermediaire_total(), by = c("Commune de résidence", 
                                              "Commune de travail")) %>%
      mutate(part_mod = `Flux domicile-travail` / `Flux total par paire de communes`,
             txt = paste(`Mode de transport`, "\nPart modale :", 
                         round(part_mod * 100, 1), "%\nTrajets :", 
                         round(`Flux domicile-travail`, 0), "/", 
                         round(`Flux total par paire de communes`, 0))) %>%
      collect()
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
  
  
  # Cet objet est "réactif" : il va créer un rendu qui se mettra à jour
  # dès qu'un élément qui le compose est modifié.
  output$matrice_od_commune_mode <- renderPlot({
    
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

  })

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
  
  
  # On récupère le centre des communes 
  centres_communes_p <- reactive({
    my_communes <- communes()
    map_df(my_communes$CODGEO, get_communes, "centre") %>%
      bind_cols(select(my_communes, LIBGEO)) %>% # on leur rattache leur nom
      # la fonction od2line requiert que le champ identifiant soit le premier à gauche
      relocate(LIBGEO, .before = geometry)
  })
  
  
  # On calcule les flux entre chaque paire de communes
  # d'abord avec 1 ligne par type de mode de transport (vélo, voiture...)
  mobpro_od <- reactive({
    matrice_od_epci() %>%
      select(`Commune de résidence`, `Commune de travail`, `Mode de transport`,
             `Flux domicile-travail`) %>%
      pivot_wider(names_from = `Mode de transport`, 
                  values_from = `Flux domicile-travail`) %>%
      filter(!is.na(`Commune de résidence`) & !is.na(`Commune de travail`)) %>%
      filter(`Commune de résidence` != `Commune de travail`) %>%
      mutate(`Nombre total de trajets` = rowSums(across(where(is.numeric)), 
                                                 na.rm = TRUE),
             `Nombre total de trajets` = round(`Nombre total de trajets`, 0))
  })

  # On utilise la fonction du od2line du package stplanr
  # A lire : https://docs.ropensci.org/stplanr/
  mobpro_od_2l <- reactive({
    od2line(flow = mobpro_od(), zones = centres_communes_p()) 
  })
  
  # Cette matrice est bidirectionnelles : le flux Nantes-Rezé se supperpose au 
  # flux Rezé-Nantes. On va cumuler les flux en utilisant la fonction od_oneway
  # puis calculer des agrégats en rassemblant les modes de transports d'intérêt
  mobpro_od_1l <- reactive({
    mobpro_od_2l() %>%
      od_oneway() %>%
      mutate(`Modes actifs (sans TC)` = rowSums(cbind(`Vélo`, Marche), 
                                                na.rm = TRUE) / 
                                                  `Nombre total de trajets`,
             `Modes durables (avec TC)` = rowSums(cbind(`Vélo`, Marche, TC), 
                                                  na.rm = TRUE) / 
                                                    `Nombre total de trajets`)
  })
  
  # Ces cartes sont plus jolies à regarder avec un fond, en mode "view"
  #tmap_mode("view")
  output$map <- renderTmap({ 
    tm_shape(mobpro_od_1l()) +
      tm_lines(palette = "RdYlGn", #viridis::plasma(10),
               lwd = "Nombre total de trajets",
               breaks = c(0:10)/10,
               scale = 9,
               title.lwd = "Nombre de trajets",
               alpha = 1,
               col = input$modes, 
               text.separator = "-",
               title = "Part modale",
               legend.lwd.show = FALSE) + # Pas de légende d'épaisseur des lignes encore dispo
      tm_layout(legend.format=list(fun=function(x) ifelse(x > 1, x,
                                                          paste0(formatC(x*100, 
                                                           digits=0, format="f"), 
                                                           " %")),
                                   text.separator = "-"),
                legend.outside = TRUE)
  })
  
  output$histo <- renderPlotly({
    # On représente la matrice origine destination. NB : Tout se passe sur les 4 
    # premières lignes (facet), la suite n'est que de la mise en forme
    my_flux <- collect(flux_epci())
    graph_od_mod <- my_flux %>%
      count(`Commune de résidence`, `Mode de transport`, wt = IPONDI) %>%
      right_join(count(my_flux, `Commune de résidence`, wt = IPONDI), 
                 by = "Commune de résidence") %>%
      mutate(part_mod = paste("Part modale :", round(n.x / n.y * 100, 1), "%",
                              "\nTrajets :", round(n.x, 0), "/", round(n.y, 0))) %>%
      ggplot(aes(x = `Commune de résidence`, weight = n.x,
                 fill = `Mode de transport`, 
                 text = part_mod)) + # A ajouter pour les modes de transport
      geom_bar(position = "fill") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y = element_blank()) +
      scale_y_continuous(labels = scales::label_percent())
    # On reproduit le graphique ggplot 'graph_mod' créé plus haut, en le passant
    # simplement à plotly avec la fonction 'ggplotly'
    ggplotly(graph_od_mod, tooltip = c("x", "fill", "text"))
  })
    
    
  
}

# On force l'application à s'exécuter dans le navigateur web 
# (pas le navigateur shiny)
options(shiny.launch.browser = TRUE)
# On lance l'application
shinyApp(ui = ui, server = server)
