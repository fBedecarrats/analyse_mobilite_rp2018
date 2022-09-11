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
library(waiter)



# On charge la liste des communes ici plutôt que dans
EPCI_FR <- read_excel("data/Recensement/Intercommunalite_Metropole_au_01-01-2018.xls", 
                      sheet = "Composition_communale", skip = 5) %>%
  select(LIBEPCI, CODGEO, LIBGEO)


ui <- fluidPage(
  autoWaiter(), # Pout avoir des menus d'attente
  titlePanel("Trajets domicile-travail (Recensement 2018)"),
  sidebarLayout(
    sidebarPanel(width = 2,
      selectInput("epci",
                  "Choisir une EPCI:",
                  choices = sort(unique(EPCI_FR$LIBEPCI)),
                  selected = "Nantes Métropole"),
      checkboxGroupInput("deplacement",
                         "Mode de déplacement",
                         c("Aucun" = "1",
                           "Marche" = "2",
                           "Vélo" = "3",
                           "Moto" = "4",
                           "Voiture"="5",
                           "TC"="6"),
                         plotlyOutput("distPlot")
                         
      )
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Matrice origine/destination", 
               #  textOutput("click_data"),
                 plotOutput("matrice_od_commune_mode", height = "650px")),#,
                            #  click = "plot_click")),
        tabPanel("Carte des flux",
                 tmapOutput("map")),
        tabPanel("Histogramme")
      )
    )
  ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  # Chargement des données
  # On utilise vroom qui est ~10x plus rapide pour lire de gros fichiers
  # Nouvelle version avec arrow, encore 10x plus rapide
  
  flux <- open_dataset("data/Recensement/FD_MOBPRO_2018.parquet")
  
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
  
  # On liste les communes à garder
  communes <- reactive({ communes_interco(interco = "Nantes Métropole") })
  # communes <- reactive({ communes_interco(interco = input$epci) })
  
  
  # On crée une variable réactive avec la matrice origine-destination sélectionnée
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
    flux_epci() %>%
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
  
  # output$click_data <- renderText({
  #   req(input$plot_click)
  #   nearPoints(matrice_od_epci(), input$plot_click) %>%
  #     pull(txt)
  # })
  
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
      select(-`Flux total par paire de communes`) %>%
      collect() %>%
      pivot_wider(names_from = `Mode de transport`, 
                  values_from = `Flux domicile-travail`) %>%
      filter(!is.na(`Commune de résidence`) & !is.na(`Commune de travail`)) %>%
      filter(`Commune de résidence` != `Commune de travail`) %>%
      mutate(total = rowSums(across(where(is.numeric))))
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
      mutate(`Modes actifs (sans TC)` = rowSums(cbind(`Vélo`, Marche), na.rm = TRUE) / total,
             `Modes durables (avec TC)` = rowSums(cbind(`Vélo`, Marche, TC), na.rm = TRUE) / total)
  })
  
  # Ces cartes sont plus jolies à regarder avec un fond, en mode "view"
  #tmap_mode("view")
  output$map <- renderTmap({ 
    tm_shape(mobpro_od_1l()) +
      tm_lines(palette = viridis::plasma(10),
               lwd = "total",
               breaks = c(0:5)/10,
               scale = 9,
               title.lwd = "Nombre de trajets",
               alpha = 1,
               col ="Modes durables (avec TC)", 
               text.separator = "-",
               title = "Part modale") # +
      # tm_layout(legend.format=list(fun=function(x) ifelse(x > 1, x,
      #                                                     paste0(formatC(x*100, digits=0, format="f"), " %")),
      #                              text.separator = "-"),
      #           legend.outside = TRUE)
  })
  
}

# On force l'application à s'exécuter dans le navigateur web 
# (pas le navigateur shiny)
options(shiny.launch.browser = TRUE)
# On lance l'application
shinyApp(ui = ui, server = server)
