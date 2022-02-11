#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)

# On charge la liste des EPCI
EPCI_FR <- read_excel("data/Intercommunalite_Metropole_au_01-01-2018.xls", 
                      sheet = "Composition_communale", skip = 5)

# Chargement des données
# On utilise des read_delim plutôt que des read_csv car ce ne sont pas 
# de 'vrais' csv : Ils correspondent à des exports csv depuis MS Excel.
flux <- read_delim("data/FD_MOBPRO_2018.csv", # le fichier INSEE 
                   delim = ";", # car ce ne sont pas de "vrais" csv
                   escape_double = FALSE, trim_ws = TRUE, # spécificité Excel
                   # On va préciser le format de certaines colonnes ici pour 
                   # éviter d'avoir à le faire plus tard
                   col_types = cols(DCFLT = col_character(),
                                    REGLT = col_character())) %>%
  # On crée des intitulés lisibles: nom de commune au lieu des numéros
  left_join(rename(EPCI_FR, `Commune de résidence` = LIBGEO), 
            by = c("COMMUNE" = "CODGEO")) %>%
  left_join(rename(EPCI_FR, `Commune de travail` = LIBGEO), 
            by = c("DCLT" = "CODGEO")) %>%
  mutate(`Mode de transport` =  fct_recode(factor(TRANS),
                                           "Aucun" = "1",
                                           "Marche" = "2",
                                           "Vélo" = "3",
                                           "Moto" = "4",
                                           "Voiture" = "5",
                                           "TC" = "6"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Matrices origines destination par EPCI"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("epci",
                        "Choisir une EPCI:",
                        choices = c("CU Angers Loire Métropole", "Nantes Métropole"),
                        #choices = unique(EPCI_FR$LIBEPCI),
                        selected = "Nantes Métropole")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("matrice_od_commune_mode")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
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
  

  
  
  

  
    output$matrice_od_commune_mode <- renderPlot({
      my_epci = input$epci
     # my_epci = "Nantes Métropole"
      # On liste les communes à garder
      communes <- communes_interco(interco = my_epci)
      # On filtre la donnée de flux pour ne garder que ces communes
      flux_epci <- flux %>%
        filter(COMMUNE %in% communes$CODGEO | DCLT %in% communes$CODGEO)
      # On affiche les commune par ordre décroissant de flux
      ordre_communes <- flux_epci %>%
        group_by(`Commune de résidence`) %>%
        summarize(flux = sum(IPONDI, na.rm = TRUE)) %>%
        arrange(desc(flux)) %>%
        filter(!is.na(`Commune de résidence`)) %>%
        pull(`Commune de résidence`)
      
      # On génère la visualisation
      flux_epci <- flux_epci %>%
        mutate(`Commune de résidence` = factor(`Commune de résidence`,
                                               levels = ordre_communes),
               `Commune de travail` = factor(`Commune de travail`,
                                             levels = ordre_communes)) 
      graph <- flux_epci %>%
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
      
    })
    flux_epci %>%
      ggplot(aes(x = "", fill = `Mode de transport`,
                 weight = IPONDI)) + # On pondère à partir de l'échantillonage
      geom_bar(position = "fill") + # Pour afficher les résultats en proportion
      facet_grid(`Commune de résidence` ~ `Commune de travail`)
}

# Run the application 
shinyApp(ui = ui, server = server)
