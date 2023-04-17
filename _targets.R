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
  tar_target(
    collab_poids, # Cible pour les df du nbr de collab entre paires d'étudiants uni et bidirectionel
    collab_poids_fun(dbpath)
  ),
  tar_target(
    matrice_adj, # Cible pour la matrice d'adjacence avec poids du réseau
    matrice_fun(arcs=collab_poids[[1]])
  ),
  tar_target(
    df_cci, # Cible pour le df des cci pour chaque noeud
    cci_fun(matrice_adj=matrice_adj)
  ),
  tar_target(
    df_noeuds, # Cible pour le df des noeuds avec informations utiles
    noeuds_fun(dbpath=dbpath,df_cci=df_cci)
  ),
  tar_target(
    stats, # Cible pour les statistiques mesurées sur le réseau
    stats_fun(dbpath=dbpath,arcs=collab_poids[[1]],arcsUniq=collab_poids[[2]],df_noeuds=df_noeuds,df_cci,matrice_adj=matrice_adj)
  ),
  tar_target(
    network, # Cible pour l'objet network utilisé pour la représentation graphique du réseau
    network_fun(arcsUniq=collab_poids[[2]],df_noeuds=df_noeuds)
  ),
  tar_target(
    noeuds_arcs,
    noeuds_arcs_fun(df_noeuds=df_noeuds,arcs=collab_poids[[1]])
  ),
  tar_render(
    rapport,
    path="rapport/test_rapport.Rmd",
    params=list(clean_data=clean_data,stats=stats)
  )
)
