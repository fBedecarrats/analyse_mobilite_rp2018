# analyse_mobilite_rp2018

This is a hands-on tutorial on how to use R+Rstudio (and Shiny) with a use case on 2018 French census data.

Ce dépôt contient des fichiers utilisés dans le cadre d'une formation-action menée à Nantes Métropole. Il s'agissait d'un temps informel mais régulier d'initiation à R, avec un groupe de 6 collègues. Il s'agissait d'une mise en oeuvre pratique, qui a débouché sur la réalisation de deux rendus :

-   Une séquence d'analyse pas à pas, réalisée sous forme de notebook (Quarto) c'est-à-dire de document d'analyse contenant à la fois les codes de traitement de données, leurs résultats (calculs et visualisation) et le texte qui présente la démarche et les résultats. Cette analyse s'est focalisée sur les données de recensement de 2018, avec des données détaillées sur 127 953 personnes qui résident ou travaillent sur le territoire de Nantes Métropole : origine, destination, mode de transport principal le plus souvent utilisé pour aller travailler, caractéristiques socio-éco-démographiques. On trouvera dans ce dépôt le code source et le rendu html.

-   Une application interactive (shiny) qui reprend les principales visualisations réalisées dans le notebook. Cet outil permet de visualiser les résultats pour l'ensemble des EPCI française, sur les années 2016, 2017, 2018 et 2019 (\> 31 millions d'observations). Le code source est dans ce dépôt et l'application est accessible ici.

Ce cas d'usage visait plusieurs objectifs : mobiliser une donnée suffisamment volumineuse pour nécessiter plus qu'un tableur, suffisamment riche pour que de multiples analyses soient pertinentes (matrice origine destination, croisements par modalité, cartographies...).

Les sources pour l'ensemble du code source et du texte du présent document est accessible sur Github à l'adresse suivante : <https://github.com/fBedecarrats>. Les analyses sont menées sur la plateforme SSP Cloud, mise à disposition par l'INSEE pour les data scientist travaillant pour des administrations publiques. Il s'agit d'une instance de stockage de données massif (S3) et de calcul haute performance (cluster Kubernetes) disposant d'une interface simplifiée permettant à l'utilisateur de configurer, lancer et administrer facilement des environnements de traitement de données (RStudio server, Jupyter lab ou autres...). Le code est conçu pour s'exécuter de la même manière en local sur un PC, mais la préparation des données sera certainement beaucoup plus longue à exécuter.

