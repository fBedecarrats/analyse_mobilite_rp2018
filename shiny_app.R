# On charge nos librairies
library(shiny) # Pour faire tourner l'application shiny
library(readr)
library(readxl)
library(ggplot2)
library(dplyr)
library(forcats)
library(vroom)

# On charge la liste des communes ici plutôt que dans
EPCI_FR <- read_excel("data/Recensement/Intercommunalite_Metropole_au_01-01-2018.xls", 
                      sheet = "Composition_communale", skip = 5) %>%
  select(LIBEPCI, CODGEO, LIBGEO)


# Choix de la structure de l'UI: navbarpage et le thème simplex de bootstrap
# cf. https://shiny.rstudio.com/articles/layout-guide.html
ui <- navbarPage(title = "Trajets domicile-travail (Recensement 2018)",
                 theme = bslib::bs_theme(bootswatch = "simplex", version = 5),
                 tabPanel("Matrice des parts modales",
                          fluidRow(
                            column(3, # 2/12, c'est la largeur de notre colonne
                                   selectInput("epci",
                                               "Choisir une EPCI:",
                                               choices = sort(unique(EPCI_FR$LIBEPCI)),
                                               selected = "Nantes Métropole")),
                            column(9, 
                                   plotOutput("matrice_od_commune_mode",
                                              height = "600px"))
                            
                          )
                 )
)

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
    output$matrice_od_commune_mode <- renderPlot({
      
      # On récupère l'EPCI sélectionnée dans l'UI
      my_epci = input$epci
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
     
      # Cette étape est factultative mais fait gagner ~2secondes de traitement
      # car dplyr est un peu plus rapide que ggplot2
      intermediaire <- flux_epci %>%
        group_by(`Mode de transport`, `Commune de résidence`, `Commune de travail`) %>%
        summarise(`Flux domicile-travail` = sum(IPONDI, na.rm = TRUE))
      
      # On génère la visualitation. 
      intermediaire %>% # marche aussi en partant de flux_epci, mais + long
        ggplot(aes(x = "", fill = `Mode de transport`)) + # On pondère à partir de l'échantillonage
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
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
