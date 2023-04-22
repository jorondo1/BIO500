### Importer tous les packages nécessaires
library(pacman)
p_load(bookdown,data.table, dplyr, GGally,ggpubr, gridExtra, Hmisc, igraph, intergraph, 
       knitr, magrittr, network, NetworkToolbox, patchwork, purrr, RColorBrewer, 
       readr, rmarkdown, RSQLite, rstatix, rticles, sparsebnUtils, sna, spaa,
       stringdist, stringr, tarchetypes, targets, tibble, tidygraph, tidyr, 
       visNetwork, wordcountaddin)

### Scripts R
source("scripts/create_db.R")
source("scripts/analyses.R")

### Pipeline
list(
  tar_target(
    name = path, # Cible
    command = "./data", # Dossier contenant les fichiers de données
    format = "file" # Format de la cible
  ),
  tar_target(
    name = file_paths, # Cible
    command = list.files(path, full.names = TRUE) # Liste les fichiers dans le dossier
  ),
  tar_target(
    data, # Cible pour les données brutes
    readData.fun(file_paths)
  ),
  tar_target(
    clean_data, # Cible pour les données nettoyées
    clean.fun(data=data)
  ),
  tar_target(
    dbpath, # Cible pour le chemin de la database
    createdb.fun(clean_data)
  ),
  tar_target(
    collab_poids, # Cible pour les df du nbr de collab entre paires d'étudiants uni et bidirectionel
    collab_poids.fun(dbpath)
  ),
  tar_target(
    matrice_adj, # Cible pour la matrice d'adjacence avec poids du réseau
    matrice.fun(arcs=collab_poids[[1]])
  ),
  tar_target(
    df_cci, # Cible pour le df des cci pour chaque noeud
    cci.fun(matrice_adj=matrice_adj)
  ),
  tar_target(
    df_noeuds_tous, # Cible pour le df de tous noeuds avec informations utiles et cci
    noeuds_tous.fun(dbpath=dbpath,df_cci=df_cci)
  ),
  tar_target(
    df_noeuds_classe, # Cible pour le df des noeuds de la classe seulement
    noeuds_classe.fun(df_noeuds_tous)
  ),
  tar_target(
    df_stats, # Cible pour les statistiques mesurées sur le réseau
    stats.fun(dbpath=dbpath,arcs=collab_poids[[1]],arcsUniq=collab_poids[[2]],df_noeuds_classe=df_noeuds_classe,df_cci,matrice_adj=matrice_adj)
  ),
  tar_target(
    network, # Cible pour l'objet network utilisé pour la représentation graphique du réseau
    network.fun(arcsUniq=collab_poids[[2]],df_noeuds_classe=df_noeuds_classe)
  ),
  tar_target(
    noeuds_arcs, # Cible pour les df de noeuds et d'arcs utilisés par la fonction VisNetwork
    noeuds_arcs.fun(df_noeuds_tous=df_noeuds_tous,arcs=collab_poids[[1]])
  ),
  tar_files_input(
    cssbib, # 09. Cible pour assurer le roulement de tar_render lorsque les fichiers .css ou .bib sont modifiés
    c("rapport/styles.css","rapport/bibliographie.bib"),
  ),
  tar_render(
    rapport,
    path="rapport/Rapport.Rmd", # Le chemin du fichier .Rmd à render
    params = list(cssbib)
  )
)
