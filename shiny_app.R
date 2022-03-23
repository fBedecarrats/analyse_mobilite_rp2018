# On charge nos librairies
library(shiny) # Pour faire tourner l'application shiny
# reactiveConsole(enabled = TRUE)
library(readxl) # pour lire les fichiers excel (liste communes/epci)
library(vroom) # pour lire les csv (très rapide pour les 'gros' csv)
library(dplyr) # pour les manipulation de données
library(ggplot2) # pour les graphs
library(plotly) # pour des graphs interactifs
library(ggiraph)
library(forcats) # pour modifier des facteurs (variables catégorielles ordonnées)


# On charge la liste des communes ici plutôt que dans
EPCI_FR <- read_excel("data/Recensement/Intercommunalite_Metropole_au_01-01-2018.xls", 
                      sheet = "Composition_communale", skip = 5) %>%
  select(LIBEPCI, CODGEO, LIBGEO)


ui <- fluidPage(
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
                 girafeOutput("matrice_od_commune_mode",
                              width = "100%", height = "80%")),
        tabPanel("Carte des flux"),
        tabPanel("Histogramme")
      )
    )
  ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  # Chargement des données
  # On utilise vroom qui est ~10x plus rapide pour lire de gros fichiers
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
  
  # Cet objet est "réactif" : il va créer un rendu qui se mettra à jour
  # dès qu'un élément qui le compose est modifié.
  output$matrice_od_commune_mode <- renderGirafe({
    # On récupère l'EPCI sélectionnée dans l'UI
    my_epci = input$epci
    # my_epci = "Nantes Métropole"
    # On liste les communes à garder
    communes <- communes_interco(interco = my_epci)
    
    # On filtre la donnée de flux pour ne garder que ces communes
    flux_epci <- flux %>%
      filter(COMMUNE %in% communes$CODGEO | DCLT %in% communes$CODGEO) %>%
      # On crée des intitulés lisibles: nom de commune au lieu des numéros
      left_join(rename(communes, `Commune de résidence` = LIBGEO), 
                by = c("COMMUNE" = "CODGEO")) %>%
      left_join(rename(communes, `Commune de travail` = LIBGEO), 
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
    
    
    
    intermediaire_total <- flux_epci %>% # on enlève le mode du group_by
      group_by(`Commune de résidence`, `Commune de travail`) %>%
      summarise(`Flux total par paire de communes` = sum(IPONDI, na.rm = TRUE))
    
    # Cette étape est factultative mais fait gagner ~2secondes de traitement
    # car dplyr est un peu plus rapide que ggplot2
    intermediaire <- flux_epci %>%
      group_by(`Mode de transport`, `Commune de résidence`, `Commune de travail`) %>%
      summarise(`Flux domicile-travail` = sum(IPONDI, na.rm = TRUE)) %>%
      left_join(intermediaire_total, by = c("Commune de résidence", 
                                            "Commune de travail")) %>%
      mutate(part_mod = `Flux domicile-travail` / `Flux total par paire de communes`,
             txt = paste(`Mode de transport`, "\nPart modale :", 
                         round(part_mod * 100, 1), "%\nTrajets :", 
                         round(`Flux domicile-travail`), "/", 
                         round(`Flux total par paire de communes`)))
    
    ## Avec ggigraph
      dynamic_plot <- intermediaire %>% # marche aussi en partant de flux_epci, mais + long
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
      girafe(ggobj = dynamic_plot, height_svg = 10, width_svg = 12,
             options = list(opts_selection(type = "single"),
                            opts_sizing(rescale = TRUE)))

  })
  
}

# On force l'application à s'exécuter dans le navigateur web 
# (pas le navigateur shiny)
options(shiny.launch.browser = TRUE)
# On lance l'application
shinyApp(ui = ui, server = server)