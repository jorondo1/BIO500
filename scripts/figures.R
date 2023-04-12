library(pacman)
p_load(tidygraph, igraph, dplyr, GGally, #sna, intergraph, sparsebnUtils, 
       network, magrittr, RColorBrewer, NetworkToolbox, tibble, rstatix, ggpubr,
       visNetwork, spaa)

#-----------------------------------------------------------------------------#
#--- Préparation des matrices d'arcs et de noeuds ----------------------------#
#-----------------------------------------------------------------------------#

# Créer une matrice d'arcs (1 arc = 1 collaboration) avec poids
# cette matrice contient les collaborations dans les deux sens

arcs <- collaborations %>%
  count(etudiant1,etudiant2) %>% # compter les collabsl
  filter(etudiant1!=etudiant2) # À RETIRER!!!!!!!!!!!!!!

# Pour travailler avec le package network, nous gardons seulement un arc par
# collaboration.
arcsUniq <- arcs %>% 
  mutate(id = case_when(etudiant1<etudiant2 ~ paste0(etudiant1,"+",etudiant2),
                        etudiant2<etudiant1 ~ paste0(etudiant2,"+",etudiant1))) %>%
  distinct(id, .keep_all=TRUE) %>% 
  mutate_if(is.character, as.factor)

arcs %<>% mutate_if(is.character, as.factor) # doivent être des facteurs pour 

#-----------------------------------------------------------------------------#
#--- Calcul du coefficient de regroupment individuel -------------------------#
#-----------------------------------------------------------------------------#

# Calculer la matrice d'adjacence avec poids, où le poids représente
# le nombre de fois que deux étudiants ont travaillé ensemble
adjPoids <- list2dist(arcs) %>% 
  as.matrix %>% replace(is.na(.),0)
adjPoids %>% isSymmetric # La matrice est symétrique!

# Calculer le CCi pour tous les noeuds
cci <- clustcoeff(adjPoids, weighted=TRUE) %$% CCi %>% 
  data.frame(cci = .) %>% 
  rownames_to_column("ID")

#!!!!! LA REQUETE SRA FAITE EN SQL, merge cci direct dans le code suivant:

# Création d'une table de données pour tester les hypothèses et produire les figures
cciGroup <- etudiants %>% arrange(by = ID) %>%
  dplyr::select(ID, formation_prealable, regime_coop, programme, annee_debut) %>% 
  left_join(cci, by="ID") %>% # ajouter le CCi
  
  # Conserver seulement les étudiants ayant au moins une des trois variables d'intérêt:
  dplyr::filter(!is.na(formation_prealable) || 
                  !is.na(annee_debut) ||
                  !is.na(regime_coop) ||
                  !is.na(programme)) %>% 
  
  # Formater les variables pour des catégories pertinentes et desfigures propres
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

#-----------------------------------------------------------------------------#
#--- Statistiques descriptives du réseau -------------------------------------#
#-----------------------------------------------------------------------------#

# Préparer les données pour la table 

# Distribution de la densité
mean(arcs$n) # en moyenne 1.95 interactions par personne
sd(arcs$n) # ±2.50 

(L <- arcsUniq %>% nrow) # nombre de collabs
(S <- etudiants %>% nrow) # nombre d'étudiants
(densite <- L/S) # Moyenne de collabs par etudiant (sous estimée)
m <-  S*(S-1)/2 # nombre d'interactions possibles dans un réseau unipartite simple non-dirigé
(C0 <- L/m) # environ 5.7 %; sous-estimée, car les interactions entre étudiants hors-cours ne sont pas toutes comptabilisées

# On peut mieux l'estimer en utilisant seulement les étudiants du cours, car 
# théoriquement toutes les interactions 
(L_core <- inner_join(x = arcs, y = comm, 
                      by = join_by(etudiant1 == ID)) %>% nrow)
(S_core <- comm %>% nrow)
(densite_core <- L_core/S_core) # Mieux estimée, car on calcule par étudiant ayant 
                            # des données complètes (ou presque)

m_core <- S_core*(S_core-1)/2
(C0_core <- L_core/m_core) 
  # environ 70% des interactions potentielles réalisées entre les gens du cours!

# Calculer le nombre d'arcs par personne
k <- arcsUniq %>% 
  group_by(etudiant1) %>% # varie légèrement si on utilise etudiant2, erreurs de saisie (devrait être symétrique)
  summarise(n=sum(n))

# Calculer le diamètre avec iGraph
d <- graph_from_adjacency_matrix(adjPoids, 
                                 mode = 'undirected',
                                 weighted = TRUE) %>% 
  simplify %>% 
  diameter(directed = FALSE) # 13

#-----------------------------------------------------------------------------#
#--- Visualisation du réseau avec Network ------------------------------------#
#-----------------------------------------------------------------------------#

# Créer l'objet network à partir de arcsUniq
net = network(arcsUniq[,1:2] %>% as.matrix, 
              directed = FALSE) 

# Fonction génératrice de palettes de couleur
colfunc <- colorRampPalette(c("grey50", "black"))
str_vec <- arcsUniq$n %>% max %>% colfunc

# Network ne travaille pas bien avec les facteurs, on retourne en charactères
comm <- cciGroup %>% dplyr::select(-cci) %>% 
  mutate_if(is.factor, as.character)

# Ajouter les variables pertinentes au network:
net %v% "regime" = comm %$% regime 
net %v% "form" = comm %$% form
net %v% "annee" = comm %$% annee
net %v% "prog" = comm %$% prog

# Vecteur de poids pour gradient de couleur :
net %e% "colWeight" <- str_vec[arcsUniq$n]

# vecteur de poids pour l'épaisseur des arcs:
net %e% "sizeWeight" <- (arcsUniq$n/(max(arcsUniq$n)))^0.6

# vecteur de transparence pour rendre les noeuds "inconnus" plus transparents:
net %v% "aNA_start" <- comm %>% 
  mutate(n=case_when(annee=="Inconnu" ~ 0.5, TRUE ~ 1)) %$% n

net %v% "aNA_form" <- comm %>% 
  mutate(n=case_when(form=="Inconnu" ~ 0.2, TRUE ~ 1)) %$% n

net %v% "aNA_regime" <- comm %>% 
  mutate(n=case_when(regime=="Inconnu" ~ 0.5, TRUE ~ 1)) %$% n

net %v% "aNA_prog" <- comm %>% 
  mutate(n=case_when(prog=="Inconnu" ~ 0.5, TRUE ~ 1)) %$% n

#-----------------------------------------------------------------------------#
#--- Visualisation interactive du réseau avec visNetwork ---------------------#
#-----------------------------------------------------------------------------#
vNodes <- data.frame(id=sort(comm$ID), 
                    label=sort(comm$ID), 
                    color = "#B3E2CD")

vEdges <- data.frame(from=arcs[,1], 
                    to=arcs[,2], 
                    value = (arcs[,3]/max(arcs[,3])^0.6),
                    color = "#FDCDAC")

