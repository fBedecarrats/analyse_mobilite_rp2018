#!/bin/sh

# Create variables
WORK_DIR=/home/rstudio/analyse_mobilite_rp2018
REPO_URL=https://github.com/fBedecarrats/analyse_mobilite_rp2018 # As initial
DATA_DIR=${WORK_DIR}/data
DOC_DIR=${WORK_DIR}/documentation

# Git
git clone $REPO_URL $WORK_DIR
chown -R rstudio:users $WORK_DIR

# Folders to store data and documentation
mkdir $DATA_DIR
chown -R rstudio:users $DATA_DIR
cd $DATA_DIR
wget https://www.insee.fr/fr/statistiques/fichier/5395749/RP2018_mobpro_csv.zip
wget https://www.insee.fr/fr/statistiques/fichier/2510634/Intercommunalite_Metropole_au_01-01-2018.zip
unzip RP2018_mobpro_csv.zip
unzip Intercommunalite_Metropole_au_01-01-2018.zip

mkdir $DOC_DIR
chown -R rstudio:users $DOC_DIR
cd $DOC_DIR
wget https://www.insee.fr/fr/statistiques/fichier/5395749/contenu_RP2018_mobpro.pdf
# The copy from SSP Cloud S3 is broken (see https://github.com/InseeFrLab/sspcloud/issues/12)
# We replace it with a wget
# copy files from S3 : SUSPENDED
# mc cp s3/fbedecarrats/diffusion/{FD_MOBPRO_2018.csv,commune2021.csv,Intercommunalite-Metropole_au_01-01-2021.xlsx,Varmod_MOBPRO_2018.csv} $DATA_DIR
# mc cp s3/fbedecarrats/diffusion/contenu_RP2018_mobpro.pdf $DOC_DIR

# launch RStudio in the right project
# Copied from InseeLab UtilitR
    echo \
    "
    setHook('rstudio.sessionInit', function(newSession) {
        if (newSession && identical(getwd(), path.expand('~')))
        {
            message('On charge directement le bon projet :-) ')
            rstudioapi::openProject('~/analyse_mobilite_rp2018')
            rstudioapi::applyTheme('Merbivore')
            }
            }, action = 'append')
            " >> /home/rstudio/.Rprofile