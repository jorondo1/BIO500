### Importer tous les packages nécessaires
library(pacman)
p_load(dplyr, RSQLite, magrittr, stringr, purrr, readr, 
       stringdist, tidyr, data.table, Hmisc,targets,visNetwork,tarchetypes,rmarkdown)

### Scripts R
source("scripts/travail_target.R")

### Pipeline
list(
  tar_target(
    data, # Cible pour les données brutes
    readData.fun()
  ),
  tar_target(
    clean_data, # Cible pour les données nettoyées
    clean.fun(data=data)
  ),
  tar_target(
    dbpath, # Cible pour le chemin de la database
    createdb.fun(clean_data)
  ),
  tar_render(
    rapport,
    path="rapport/Rapport_Final.Rmd",
    params=list(clean_data=clean_data,dbpath=dbpath)
  )
)
