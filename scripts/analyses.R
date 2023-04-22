##########################···
#### Contenu du script ####···
##########################···

# 01. Fonction création df des arcs avec poids
# 02. Fonction création matrice d'adjacence avec poids
# 03. Fonction création df du cci pour chaque etudiants
# 04. Fonction création du df des noeuds (tous les étudiants) avec cci et 
#     informations utilisées pour tester hypothèses et produire graphiques
# 05. Fonction sélection des noeuds de la classe du df des noeuds
# 06. Fonction création des statistiques descriptives du réseau
# 07. Fonction création de l'object network utilisé pour le graphique du réseau
# 08. Fonction création des df de noeuds et d'arcs utilisés pour le graphique 
#     interactif avec VisNetwork

# ##############################################################################
# #### 01. Fonction création df des arcs avec poids ############################
# ##############################################################################

collab_poids.fun <- function(dbpath) {
  
  ### Créer un df des arcs pour la matrice de réseau avec poids 
  #(1 arc pour chaque paire d'étudiants et le poids représente le nbr de collab)
  con<- dbConnect(SQLite(), dbname=dbpath) #Connexion avec la db
  ### Ce df contient les collaborations dans les deux sens
  arcs <- dbGetQuery(con,
                     "SELECT etudiant1, etudiant2, COUNT(sigle) AS n 
                     FROM collaborations 
                     GROUP BY etudiant1, etudiant2;") # compter les collabs
  
  # Pour travailler avec le package network, nous gardons seulement un arc par
  # collaboration.
  arcsUniq <- arcs %>% 
    mutate(id = case_when(etudiant1<etudiant2 ~ paste0(etudiant1,"+",etudiant2),
                          etudiant2<etudiant1 ~ paste0(etudiant2,"+",etudiant1))) %>% 
    distinct(id, .keep_all=TRUE) %>% 
    mutate_if(is.character, as.factor) # doivent être des facteurs pour certaines fonctions utilisées ultérieurement
  
  arcs %<>% mutate_if(is.character, as.factor) # doivent être des facteurs pour certaines fonctions utilisées ultérieurement
  dbDisconnect(con) # Déconnexion de la db
  return(list(arcs,arcsUniq))
}

# ##############################################################################
# #### 02. Fonction création matrice d'adjacence avec poids ####################
# ##############################################################################

matrice.fun <- function (arcs) {
  ### Calculer la matrice d'adjacence avec poids, où le poids représente
  # le nombre de fois que deux étudiants ont travaillé ensemble
  adjPoids <- list2dist(arcs) %>% 
    as.matrix %>% replace(is.na(.),0)
  adjPoids %>% isSymmetric # La matrice est symétrique! 
  return(adjPoids)
}

# ##############################################################################
# #### 03. Fonction création df du cci pour chaque etudiants ###################
# ##############################################################################

cci.fun <- function (matrice_adj) {
  # Calculer le CCi pour tous les noeuds
  cci <- clustcoeff(matrice_adj, weighted=TRUE) %$% CCi %>% 
    data.frame(cci = .) %>% 
    rownames_to_column("ID")
  return(cci)
}

# ##############################################################################
# #### 04. Fonction création du df des noeuds (tous les étudiants) avec cci et #
# #### informations utilisées pour tester hypothèses et produire graphiques ####
# ##############################################################################

noeuds_tous.fun <- function (dbpath,df_cci) {
  con<- dbConnect(SQLite(), dbname=dbpath) #Connexion avec la db
  # Création d'un df des étudiants (noeuds) et de leurs données utiles pour tester 
  #les hypothèses et produire les figures
  df_noeuds_tous <- dbGetQuery(con,"
                         SELECT ID, formation_prealable, regime_coop, programme, annee_debut 
                         FROM etudiants") 
  df_noeuds_tous <- left_join(df_noeuds_tous,df_cci, by="ID") %>% # ajouter le CCi
    
    # Formater les variables pour des catégories pertinentes et des figures propres
    mutate(annee = case_when(is.na(annee_debut) ~ "Inconnu",
                             annee_debut == "A2020" ~ "A2020",
                             TRUE ~ "Autre"),
           prog = case_when(is.na(programme) ~ "Inconnu",
                            TRUE ~ programme),
           regime = case_when(regime_coop == "TRUE" ~ "COOP",
                              regime_coop == "FALSE" ~ "Régulier",
                              is.na(regime_coop) ~ "Inconnu"),
           form = case_when(is.na(formation_prealable) ~ "Inconnu",
                            TRUE ~ formation_prealable), .keep="unused") %>% 
    mutate_if(is.character,as.factor) %>% ungroup
  dbDisconnect(con) # Déconnexion de la db
  return(df_noeuds_tous)
}

# ##############################################################################
# #### 05. Fonction sélection des noeuds de la classe du df des noeuds #########
# ##############################################################################

noeuds_classe.fun <- function (df_noeuds_tous) {
  # Conserver seulement les étudiants ayant au moins une des quatre variables d'intérêt
  df_noeuds_classe <- df_noeuds_tous %>% mutate_if(is.factor,as.character) %>% dplyr::filter(
    form!="Inconnu" |
      annee!="Inconnu" |
      regime!="Inconnu" |
      prog!="Inconnu")
  return(df_noeuds_classe)
}

# ##############################################################################
# #### 06. Fonction création des statistiques descriptives du réseau ###########
# ##############################################################################

stats.fun <- function(dbpath,arcs,arcsUniq,df_noeuds_classe,df_cci,matrice_adj) {
  con<- dbConnect(SQLite(), dbname=dbpath) #Connexion avec la db
  L <- arcsUniq %>% nrow # nombre d'intéractions au sein du réseau 
  S <- dbGetQuery(con,"SELECT count(ID) FROM etudiants;") %>% as.vector() %>% as.numeric() # nombre d'étudiants
  densite <- L/S # Moyenne d'interactions par etudiant (sous estimée)
  m <-  S*(S-1)/2 # nombre d'interactions possibles dans un réseau unipartite simple non-dirigé
  C0 <- L/m # environ 5.7 %; sous-estimée, car les interactions entre étudiants hors-cours ne sont pas toutes comptabilisées
  
  # On peut mieux l'estimer en utilisant seulement les étudiants du cours, car 
  # théoriquement toutes les interactions 
  L_core <- inner_join(x = arcs, y = df_noeuds_classe, 
                       by = join_by(etudiant1 == ID)) %>% 
    dplyr::filter(etudiant2 %in% df_noeuds_classe$ID) %>% # Conserver uniquement interactions entre 2 étudiants du cours
    nrow
  S_core <- df_noeuds_classe %>% nrow
  densite_core <- L_core/S_core # Mieux estimée, car on calcule par étudiant ayant 
  # des données complètes (ou presque)
  
  m_core <- S_core*(S_core-1)/2 # nombre d'interactions possibles dans un réseau des étudiants du cours unipartite simple non-dirigé
  C0_core <- L_core/m_core 
  # environ 43% des interactions potentielles réalisées entre les gens du cours!
  
  dbDisconnect(con) # Déconnexion de la db
  df_stats <- data.frame(tous=c(L,S,densite,m,C0),noyau=c(L_core,S_core,densite_core,m_core,C0_core))
  rownames(df_stats) <-c("Nombre d'interactions (L)","Nombre d'étudiants (S)","Densité du réseau (L/S)","Nombre d'interactions possibles au sein du réseau","Connectance du réseau")
  return(df_stats)
} 

# ######################################################################################
# #### 07. Fonction création de l'object network utilisé pour le graphique du réseau ###
# ######################################################################################

network.fun <- function (arcsUniq,df_noeuds_classe) {
  # Créer l'objet network à partir de arcsUniq
  net = network(arcsUniq[,1:2] %>% as.matrix, 
                directed = FALSE) 
  
  # Fonction génératrice de palettes de couleur
  colfunc <- colorRampPalette(c("grey50", "black"))
  str_vec <- arcsUniq$n %>% max %>% colfunc
  
  # Network ne travaille pas bien avec les facteurs, on retourne en charactères
  df_noeuds_classe <- df_noeuds_classe %>% 
    mutate_if(is.factor, as.character)
  
  # Ajouter la variable annee aux noeuds
  net %v% "annee" = df_noeuds_classe %$% annee
  
  # Vecteur de poids pour gradient de couleur :
  net %e% "colWeight" <- str_vec[arcsUniq$n]
  
  # vecteur de poids pour l'épaisseur des arcs:
  net %e% "sizeWeight" <- (arcsUniq$n/(max(arcsUniq$n)))^0.6
  
  # vecteur de transparence pour rendre les noeuds "inconnus" plus transparents:
  net %v% "aNA_start" <- df_noeuds_classe %>% 
    mutate(n=case_when(annee=="Inconnu" ~ 0.4, TRUE ~ 1)) %$% n
  
  return(net)
}

# ##############################################################################
# #### 08. Fonction création des df de noeuds et d'arcs utilisés ###############
# #### pour le graphique interactif avec VisNetwork ############################
# ##############################################################################

noeuds_arcs.fun <- function (df_noeuds_tous,arcs) {
  # Créer un df des noeuds du réseau (étudiants)
  vNodes <- data.frame(id=sort(df_noeuds_tous$ID), 
                       label=sort(df_noeuds_tous$ID), 
                       color = "#B3E2CD")
  
  # Créer un df des arcs du réseau
  vEdges <- data.frame(from=arcs[,1], 
                       to=arcs[,2], 
                       value = (arcs[,3]/max(arcs[,3])^0.6), # Le nbr de collaborations pour chaque interaction (arc) a été
                       color = "#FDCDAC")                    # normalisé avec un exposant de 0.6 choisi par essai-erreur pour que
  return(list(vNodes,vEdges))                                # l'interval d'épaisseur des arcs soit visuellement clair dans le graphique
}


