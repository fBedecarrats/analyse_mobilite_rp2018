library(ggplot2)
stats %>%
  ungroup() %>%
  select(-`Commune de travail`) %>%
  rename(`Commune de travail` = `Nom de la commune`) %>%
  ggplot(aes(x = `Commune de travail`, weight = `Part des travailleurs qui résident dans la commune`)) +
  geom_bar() + 
  theme()
library(forcats)
flux_nm <- flux_nm %>%
  mutate(
    `Mode de transport` = as.character
           fct_recode(as.character(TRANS), c(Aucun = "1",
                                               Marche = "2",
                                               Vélo = "3",
                                               Moto = "4",
                                               Voirure = "5",
                                               TC = "6")))
levels(flux_nm$`Mode de transport`)
  ggplot(aes(x = `Commune de travail`, fill = `Mode de transport`)) +
  geom_bar()

  
  