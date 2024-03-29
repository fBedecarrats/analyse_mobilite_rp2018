library(arrow)
library(dplyr)
library(vroom)
library(forcats)

# On référence les URL des fichiers source
url_mobpro_2019 <- "https://www.insee.fr/fr/statistiques/fichier/6456056/RP2019_mobpro_csv.zip"
url_mobpro_2018 <- "https://www.insee.fr/fr/statistiques/fichier/5395749/RP2018_mobpro_csv.zip"
url_mobpro_2017 <- "https://www.insee.fr/fr/statistiques/fichier/4507890/RP2017_mobpro_csv.zip"
url_mobpro_2016 <- "https://www.insee.fr/fr/statistiques/fichier/4171531/RP2016_mobpro_csv.zip"
url_mobpro_2016 <- "https://www.insee.fr/fr/statistiques/fichier/4171531/RP2016_mobpro_csv.zip"

# On étend le timeout pour télécharger les données
options(timeout = max(300, getOption("timeout")))

years <-c(2016:2019)

flux_all <- tibble()
for (i in years) {

  print(paste("téléchargement du fichier INSEE pour", i))
  download.file(eval(parse(text=paste0("url_mobpro_", i))),
                destfile = paste0("temp_", i, ".zip"))
  print(paste("dézip du fichier INSEE pour", i))
  unzip(zipfile = paste0("temp_", i, ".zip"),
        files = paste0("FD_MOBPRO_", i, ".csv"))
  print(paste("Chargement pour", i))
  flux <- vroom(paste0("FD_MOBPRO_", i, ".csv"),
                col_types = cols_only(
                  COMMUNE = "character",
                  DCLT = "character",
                  IPONDI = "numeric",
                  TRANS = "character")) %>%
    mutate(`Mode de transport` =  fct_recode(TRANS,
                                             "Aucun" = "1",
                                             "Marche" = "2",
                                             "Vélo" = "3",
                                             "Moto" = "4",
                                             "Voiture" = "5",
                                             "TC" = "6"),
           annee = i)
  flux_all <- bind_rows(flux_all, flux)
  file.remove(paste0("temp_", years[i], ".zip"))
  file.remove(paste0("FD_MOBPRO_", years[i], ".csv"))
  rm(flux)
}


download.file("https://www.insee.fr/fr/statistiques/fichier/2510634/Intercommunalite_Metropole_au_01-01-2019.zip",
              destfile = "temp.zip")
unzip("temp.zip")
file.remove("temp.zip")
# On charge la liste des communes de 2018 (on pourrait en avoir plusieurs verisons)
EPCI_FR <- read_excel("Intercommunalite_Metropole_au_01-01-2019.xls", 
                      sheet = "Composition_communale", skip = 5) %>%
  select(LIBEPCI, CODGEO, LIBGEO)

flux_all <- flux_all %>%
  left_join(rename(EPCI_FR, `Commune de résidence` = LIBGEO, 
                   epci_resid = LIBEPCI), 
                  by = c("COMMUNE" = "CODGEO")) %>%
  left_join(rename(EPCI_FR, `Commune de travail` = LIBGEO,
                   epci_travail = LIBEPCI), 
            by = c("DCLT" = "CODGEO"))


write_parquet(flux_all, paste0("data/Recensement/FD_MOBPRO_", years[1],"_" ,
                               years[length(years)], ".parquet"))

