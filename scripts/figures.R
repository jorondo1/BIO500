library(pacman)
p_load(tidygraph, igraph, dplyr, #GGally, sna, intergraph, sparsebnUtils, 
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

# Distribution de la densité
mean(arcs$n) # en moyenne 1.95 interactions par personne
sd(arcs$n) # ±2.50 

(L <- nrow(arcs)/2) # /2 parce que tout est (théoriquement) doublé
(S <- nrow(etudiants))
(densite <- L/S)
m <-  S*(S-1)/2 # nombre d'interactions possibles dans un réseau unipartite simple non-dirigé
(C0 <- L/m) # environ 5.7 %; sous-estimée, car les interactions entre étudiants hors-cours ne sont pas toutes comptabilisées

# Calculer le nombre d'arcs par personne
k <- arcsUniq %>% 
  group_by(etudiant1) %>% # varie légèrement si on utilise etudiant2, erreurs de saisie (devrait être symétrique)
  summarise(n=sum(n))
