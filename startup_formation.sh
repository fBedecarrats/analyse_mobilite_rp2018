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

# Alternative data loading procedure for when S3 is down.
# cd $DATA_DIR
# wget https://www.insee.fr/fr/statistiques/fichier/5395749/RP2018_mobpro_csv.zip
# wget https://www.insee.fr/fr/statistiques/fichier/2510634/Intercommunalite_Metropole_au_01-01-2018.zip
# unzip RP2018_mobpro_csv.zip
# unzip Intercommunalite_Metropole_au_01-01-2018.zip

mkdir $DOC_DIR
chown -R rstudio:users $DOC_DIR

# Alternative data loading procedure for when S3 is down.
# cd $DOC_DIR
# wget https://www.insee.fr/fr/statistiques/fichier/5395749/contenu_RP2018_mobpro.pdf

# copy files from S3 
mc cp s3/fbedecarrats/diffusion/mobilites/Recensement/* /home/rstudio/analyse_mobilite_rp2018/data/
mc cp s3/fbedecarrats/diffusion/mobilites/Geovelo/* $DATA_DIR
mc cp s3/fbedecarrats/diffusion/mobilites/Documentation/* $DOC_DIR

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
