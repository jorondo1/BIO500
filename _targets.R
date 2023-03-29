### Importer tous les packages nécessaires
library(pacman)
p_load(dplyr, RSQLite, magrittr, stringr, purrr, readr, 
       stringdist, tidyr, data.table, Hmisc,targets,visNetwork,tarchetypes,rmarkdown)

### Scripts R
source("scripts/travail_target.R")

### Pipeline
list(
  tar_target(
    data,
    readData.func(lire.csv)
  ),
  tar_target(
    clean_data,
    clean.func(data=data)
  ),
  tar_target(
    connextion,
    conDB.func()
  ),
  tar_target(
    db_vide,
    createDB.func(connexion) ###TROUVER COMMENT ENREGISTRER UNE DB
  ),
  tar_render(
    rapport,
    path="rapport/rapport.Rmd",
    params=list(clean_data=clean_data)
  )
)