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
  mutate(`Mode de transport` = factor(TRANS),
         `Mode de transport` =  fct_recode(`Mode de transport`,
                                           "Aucun" = "1",
                                           "Marche" = "2",
                                           "Vélo" = "3",
                                           "Moto" = "4",
                                           "Voirure" = "5",
                                           "TC" = "6"))
flux_nm %>%
  ggplot(aes(x = `Commune de travail`, fill = `Mode de transport`)) +
  geom_bar()

flux_nm <- flux %>%
  rename(`Commune de travail` = DCLT, `Commune de résidence` = COMMUNE) %>%
  filter(`Commune de travail` %in% communes_NM$`Numéro de commune`) %>%
  left_join(communes_NM, by = c("Commune de travail" = "Numéro de commune"))

flux_nm <- flux %>%
  rename(`Commune de travail` = DCLT, `Commune de résidence` = COMMUNE) %>%
  mutate(`Commune de travail` = ifelse(`Commune de travail` %in% communes_NM$`Numéro de commune`,
                                       "Autre", `Commune de travail`),
         )
  filter(`Commune de travail` %in% communes_NM$`Numéro de commune`) %>%
  left_join(communes_NM, by = c("Commune de travail" = "Numéro de commune"))


flux_nm %>%
  ggplot(aes(x = `Commune de travail`, fill = `Mode de transport`)) +
  geom_bar()
  