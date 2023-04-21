### Importer tous les packages nécessaires
library(pacman)
p_load(data.table, dplyr, GGally, ggpubr, Hmisc, igraph, magrittr, network, 
       NetworkToolbox, purrr, RColorBrewer, readr, RSQLite, rstatix, 
       stringdist, stringr, tibble, tidygraph, tidyr, 
       spaa, visNetwork)

##########################···
#### Contenu du script ####···
##########################···

# 01.Fonctions importation des données
# 02.Fonctions nettoyage et assemblage des données
#       Collaborations
#       Etudiants
#       Cours
#       Cohérence entre les variables partagées
#         Cohérence collaborations - cours
#         Cohérence collaborations - étudiants
# 03.Fonction création et injection de la bd
# 04.Fonction création matrice de réseau

################################################################################
#### 01.Fonctions importation des données ######################################
################################################################################

# Fonction de lecture des fichiers de données
lirecsv.fun <- function(path) {
  myList <- list() # Liste vide à populer
  for (file in path) { # boucle sur chaque fichier 
    # deux types de séparateurs existent:
    sep <- ifelse(grepl(';',readLines(file, n=1)), ';', ',') 
    # lire le fichier et ajouter le df à la liste:
    myList[[which(path==file)]] <- read.csv(file, sep=sep) 
  }
  return(myList)
}

# Fonction pour créer une liste de 3 listes comportant les fichiers de données de cours, de collaboration et d'étudiants
readData.fun <- function () {
  pathCours <- Sys.glob("data/*cours*csv") # Créer un vecteur comporant le nom de tous les fichiers de données de cours
  pathCollab <- Sys.glob("data/*collaboration*csv") # Créer un vecteur comporant le nom de tous les fichiers de données de collaboration
  pathEtudiant <- Sys.glob("data/*etudiant*csv") # Créer un vecteur comporant le nom de tous les fichiers de données d'étudiants
  list_data <- list() # Liste vide qui contiendra les 3 listes de data frames
  list_data[[1]] <- lirecsv.fun(pathCours)
  list_data[[2]] <- lirecsv.fun(pathCollab)
  list_data[[3]] <- lirecsv.fun(pathEtudiant)
  return(list_data)
}

################################################################################
#### 02.Fonction nettoyage et assemblage des données ##########################
################################################################################

#####################···
### COLLABORATIONS ###···
#####################···

clean.fun <- function(data) {
  ### Importer les fichiers de collaborations de toutes les équipes
  listCollab <- data[[2]]
  # Colonnes d'intérêt :
  colCollab <- c("etudiant1","etudiant2","sigle","session")
  
  ### Retirer les caractères weird, car il y a de beaux espaces insécables <a0>
  listCollab %<>% lapply(function(df) { # s'applique à chaque élément de la liste
    # conserver seulement les colonnes d'intérêt
    df <- df[, colCollab]
    # retirer "<a0>" par colonne
    df[] <- lapply(df, function(col) gsub("<a0>", "", col))
    return(df)
  })
  
  ### Réunir tous les df de données de collaborations dans un seul df
  coll_tous <- as.data.frame(rbindlist(listCollab, use.names=TRUE))
  
  # L'équipe 4 a rapporté une erreur dans 6 de ses entrées, qu'on corrige manuellement
  # parce que ce sont les seules entrées où 'etudiant1' et 'etudiant2' sont présentes et 'sigle' et 'session' sont manquantes.
  coll_tous[coll_tous$etudiant1 %nin% c(NA,"") & coll_tous$sigle %in% "",3:4] <- c("ECL615","E2022")
  
  ### Vérifier si des données sont manquantes pour 'etudiant1', 'etudiant2', 'sigle' ou 'session'
  coll_tous[coll_tous$etudiant1 %in% c(NA,""),]
  coll_tous[coll_tous$etudiant2 %in% c(NA,""),] # Certaines entrées n'ont pas d'information pour etudiants 1 et 2
  coll_tous <- coll_tous[coll_tous$etudiant1 %nin% c(NA,""),] # Retirer les entrées de collaboration dont etudiant 1 est manquant
  coll_tous[coll_tous$sigle %in% c(NA,""),] # Aucune donnée de 'sigle' manquante
  coll_tous[coll_tous$session %in% c(NA,""),] # Aucune donnée de 'session' manquante
  
  ### Vérifier s'il n'y a pas eu d'erreur entre la colonne 'sigle' et la colonne 'session'
  unique(coll_tous$sigle)[nchar(unique(coll_tous$sigle))!=6] # Deux erreurs présentes dans la colonne sigle :
  #espace après INS154
  #et présence d'un code de session plutôt que d'un sigle
  coll_tous$sigle[coll_tous$sigle %in% "INS154 "] <- "INS154" # Retirer l'espace suivant le sigle INS154
  coll_tous <- coll_tous[coll_tous$sigle %nin% "E2022",] # Retirer les entrées qui ne contiennent pas de sigle, mais plutôt un doublon de la session
  unique(coll_tous$session)[nchar(unique(coll_tous$session))!=5] # Une erreur présente dans la colonne session : présence d'un sigle
  coll_tous$session[coll_tous$session %in% "ECL615"] <- NA # Retirer ces données mais garder les entrées puisqu'une collaboration peut être intéressante
  #même si sa session est inconnue
  
  ### Retirer les duplicats des données
  coll_tous <- coll_tous %>% unique
  
  #####################···
  ### ETUDIANTS ########···
  #####################···
  
  listEtudiant <- data[[3]]
  
  # Colonnes d'intérêt
  colEtudiant <- c("prenom_nom","prenom","nom","region_administrative",
                    "regime_coop","formation_prealable","annee_debut","programme")
  
  ### Retirer les caractères weird
  listEtudiant %<>% lapply(function(df) {
   # retirer les points des noms de colonnes
   names(df) <- sub("\\.", "", names(df))
   # conserver seulement les colonnes d'intérêt
   df <- df[, colEtudiant]
   # remove "<a0>" from all columns
   df[] <- lapply(df, function(col) gsub("<a0>", "", col))
   return(df)
  })
  
  # colliger la liste en un seul df
  etu_tous <- rbindlist(listEtudiant) %>%
   # prenom_nom devient ID, car cette variable sera utilisée comme identifiant unique
   mutate(ID=prenom_nom, .keep="unused")
    # # assigner NA aux cellules vides:
  etu_tous[etu_tous==""] <- NA
  # enlever les rangées sans identifiant:
  etu_tous %<>% drop_na(ID)
    # # Création d'une matrice de dissimilarité pour comparer tous les noms ensemble
  # et identifier les plus similaires grâce à la distance Damerau-Levenshtein
  # pour trouver les fautes de frappe.
  etudiant_u <- etu_tous$ID %>% unique # liste unique de tous les étudiants
  dist <- etudiant_u %>% stringdistmatrix(.,.,method = "dl") # matrice de distance
    # # Extraire la liste des noms
  (prob <- etudiant_u[which(dist<5 & dist > 0, arr.ind=TRUE)[,1]] %>% sort %>% as.matrix)
  # Attention, certains noms sont simplement similaires et
  # n'ont pas besoin d'être remplacés!
    # # On crée une liste des noms mal épelés
  prob_unique <- c(
   "amelie_harbeck-bastien", "ariane_barrette","cassandra_godin", "edouard_nadon-beaumier",
   "francis_boily", "ihuoma_elsie_ebere", "jonathan_rondeau-leclaire", "kayla_trempe-kay",
   "juliette_meilleur", "laurianne_plante", "louis-philippe_theriault", "mael_guerin",
   "marie_bughin", "marie-christine_arseneau", "penelope_robert", "mia_carriere",
   "philippe_barrette", "philippe_bourassa","sabrina_leclercq", "samuel_fortin",
   "sara-jade_lamontagne", "yanick_sageau" )
  
  # substitution fuzzy
  # cette loop imprime aussi les détails de la substitution pour s'assurer
  # que deux noms réellement similaires ne sont pas remplacés par le même nom
  for (i in prob_unique) {
   toReplace <- agrep(i,etu_tous$ID, ignore.case = FALSE,
                      max.distance = 2) # liste d'indices avec match fuzzy
   print(c(etu_tous[toReplace,"ID"],i))
   etu_tous[toReplace,"ID"] <- i # remplacer avec la bonne valeur
  }
  
  # # Correction à la main d'un cas particulier
  etu_tous$ID[etu_tous$ID == "roxanne_bernier\t\t\t\t\t\t\t"] <- "roxanne_bernier"
  
  ### On décide d'enlever les colonnes nom et prénom car elles sont loin d'être
  ### essentielles et contiennent potentiellement aussi des erreurs.
  etu_tous %<>% select(-c(nom, prenom))%>%
   mutate(countNA = rowSums(is.na(.))) %>% # somme des données NA par rangée
   group_by(ID) %>% # groupement pour la prochaine étape
   filter(countNA == min(countNA)) %>% # garder les rangées avec le plus de données
   distinct(ID, # retirer les entrées dupliquées;
            .keep_all=TRUE) %>% # les ambiguités sont sélectionnées arbitrairement
   # Traduire tous les booléens en anglais:
   mutate(regime_coop = case_when(
     regime_coop =="VRAI" ~ "TRUE",
     regime_coop == "FAUX" ~ "FALSE")) %>% dplyr::select(-countNA)
  
  etu_tous$region_administrative %>% unique
  # Certaines régions administratives sont mal écrites!
  
  # Régions administratives du Québec:
  regAd <- c("monteregie", "saguenay-lac-saint-jean", "mauricie", "lanaudiere",
            "estrie", "outaouais", "centre-du-quebec", "bas-saint-laurent",
            "gaspesie_iles_de_la_madeleine", "montreal", "laurentides",
            "abitibi-temiscamingue", "laval", "cote_nord", "nord_du_quebec",
            "chaudiere_appalaches", "capitale-nationale")
  
  # Boucle permettant de remplacer des erreurs de frappe mineures grâce au fuzzy match
  etu_corr <- etu_tous
  for (i in regAd) {
   toReplace <- agrep(i,etu_corr$region_administrative) # liste d'indices avec match fuzzy
   etu_tous[toReplace,"region_administrative"] <- i # remplacer avec la bonne valeur
  }
  
  # Vérifier la valeur originale des données corrigées
  etu_tous[which(etu_tous$region_administrative !=
                  etu_corr$region_administrative),
          "region_administrative"] # c'était bel et bien une erreur!
  
  # #####################···
  # ### COURS ############···
  # #####################···
  
  listCours <- data[[1]]
  
  # Enlever le i trémas que certains voient (mais pas sur mon mac!)
  names(listCours[[4]]) <- c("sigle","optionnel", "credits")
  
  ### Retirer la colonne 'optionnel' car le même cours peut être optionnel pour
  # certains étudiants et non optionnel pour d'autres, selon leur programme
  colCours <- c("sigle", "credits")
  listCours %<>% lapply(function(x) x[(names(x) %in% colCours)])
  
  ### Joindre tous df cours de toutes les équipes en un un seul df
  cours_tous <- as.data.frame(rbindlist(listCours, use.names=TRUE))
  
  ### Retirer les rangées du df cours qui ne contiennent pas de données dans la colonne 'sigle'
  cours_tous <- cours_tous[!cours_tous$sigle %in% c("",NA),]
  
  ### Trouver les sigles de cours n'existant pas en comparant à la liste de
  # cours officielle du bac en Biologie de l'UdeS
  cours_tous$sigle[order(cours_tous$sigle)] #les cours TSB302 et BIO400 n'existent pas
  
  ### Corriger les sigle erronés à la fois pour cours et coll_tous
  cours_tous$sigle[cours_tous$sigle == "TSB302"] <- "TSB303" # Modifier TSB302 en TSB303 pour df cours
  coll_tous$sigle[coll_tous$sigle == "TSB302"] <- "TSB303" # Modifier TSB302 en TSB303 pour df coll_tous
  cours_tous$sigle[cours_tous$sigle == "BIO400"] <- "BOT400" # Modifier BIO400 en BOT400 pour df cours
  coll_tous$sigle[coll_tous$sigle == "BIO400"] <- "BOT400" # Modifier BIO400 en BOT400 pour df coll_tous
  
  ### Trouver les sigles dupliqués avec des crédits différents
  nrow(unique(cours_tous))-length(unique(cours_tous$sigle)) #Calculer le nbr d'erreurs de crédits dans les données
  unique(cours_tous)[duplicated(unique(cours_tous)[,1]),1] # Identifier les cours affichant plusieurs options de crédits
  
  ### Modifier les valeurs de crédits erronnées pour les sigles dupliqués trouvés
  cours_tous$credits[cours_tous$sigle == "BIO109"] <- 1
  cours_tous$credits[cours_tous$sigle == "ECL515"] <- 2
  cours_tous$credits[cours_tous$sigle == "TSB303"] <- 2
  cours_tous$credits[cours_tous$sigle == "BOT400"] <- 1
  
  ### Valider que les sigles dupliqués avec des crédits différents ont été corrigés
  nrow(unique(cours_tous))-length(unique(cours_tous$sigle)) # 0 = all good!
  
  ################################################################################
  #### 02.Cohérence des variables partagées ######################################
  ################################################################################
  
  #########################################···
  ### Cohérence collaboration - cours ######···
  #########################################···
  
  # Trouver les sigles présents dans cours, mais pas dans coll_tous:
  (orphelins <- unique(cours_tous$sigle)[unique(cours_tous$sigle) %nin%
                                     unique(coll_tous$sigle)])
  # Il y a 10 cours présents dans cours qui n'ont pas donné lieu a des coll_tous
  
  # Retirer les cours présents dans cours mais pas dans coll_tous
  # car ces cours ne présentaient pas de coll_tous:
  cours_tous <- cours_tous[cours_tous$sigle %nin% orphelins,]
  
  # Trouver les sigles présents dans coll_tous absents de cours
  unique(coll_tous$sigle)[unique(coll_tous$sigle) %nin% unique(cours_tous$sigle)]
  # 4 erreurs sont présentes dans les sigles de coll_tous
  
  # Retirer les coll_tous relatives à GBI105... il n'y avait pas de coll_tous dans ce cours:
  coll_tous <- coll_tous[!coll_tous$sigle == "GBI105",]
  
  # Modifier le sigle GAE500 par GAE550:
  coll_tous$sigle[coll_tous$sigle == "GAE500"] <- "GAE550"
  # Modifier le sigle ECL405 pour ECL404
  coll_tous$sigle[coll_tous$sigle == "ECL405"] <- "ECL404"
  
  # Renommer le df cours_tous et filtrer les duplicats
  cours <- cours_tous %>% unique
  
  ###########################################···
  ### Cohérence collaboration - étudiants ####···
  ###########################################···
  
  # Vérifier la différence entre les identifiants étudiants:
  setdiff(etu_tous$ID, coll_tous$etudiant1 %>% unique)
  
  # Vérifier la différence entre (etudiant1 ou etudiant2) et ID
  setdiff(coll_tous$etudiant1 %>% unique, etu_tous$ID)
  setdiff(coll_tous$etudiant2 %>% unique, etu_tous$ID)
  # Plusieurs étudiants ont des erreurs de frappe!
  
  # On remplace les autres erreurs avec un fuzzy match :
  id <- etu_tous$ID
  for (i in id) {
   toReplace <- agrepl(i, # ID à fuzzy-matcher
                       coll_tous$etudiant1, # liste à chercher
                       ignore.case = FALSE,
                       max.distance = 0.07) # très sensible pour éviter de mélanger les noms similaires
   coll_tous[toReplace,"etudiant1"] <- i # remplacer avec le bon ID
  }
  # Une autre fois pour la colonne etudiant2
  for (i in id) {
   toReplace <- agrepl(i,coll_tous$etudiant2, ignore.case = FALSE, max.distance = 0.07)
   coll_tous[toReplace,"etudiant2"] <- i
  }
  
  # Ajouter les étudiants présents dans collaboration mais absents de etudiant
  # à la table etudiant
  setdiff(coll_tous$etudiant1, id)
  setdiff(coll_tous$etudiant2, id)
  setdiff(id, coll_tous$etudiant1)
  setdiff(id, coll_tous$etudiant2)
  
  # Les autres cas sont absents de etu_tous, on les ajoute:
  manquants <- data.frame(ID=setdiff(coll_tous$etudiant1 %>% unique, id))
  
  # on rassemble le tout dans un seul df
  etudiants <- rbind(etu_tous, manquants)
  
  # Sanity check ultime :
  setdiff(coll_tous$etudiant1,etudiants$ID)
  setdiff(coll_tous$etudiant2,etudiants$ID)
  setdiff(etudiants$ID,coll_tous$etudiant1)
  setdiff(etudiants$ID,coll_tous$etudiant2)
  
  # 100% des ID sont cohérents!
  
  ### Vérifier s'il y a des collaborations qui on été entrées plusieurs fois avec des 'session' différentes
  #On ne pouvait pas le faire avant de corriger les erreurs de frappes présentes dans les noms des étudiants
  sum(duplicated(coll_tous[,-4])) # En effet il y a 251 'doublons' de ce type
  coll_tous$session[apply(coll_tous[,-4],1,paste,collapse="") %in% apply(coll_tous[duplicated(coll_tous[,-4]),-4],1,paste,collapse="")] <- NA # Retirer les données de 'session' de ces entrées, car ce n'est
  #pas possible de déterminer quelle session est la bonne
  coll_tous <- coll_tous %>% unique # Retirer ces 'doublons'
  
  # renommer la table finale de collaborations
  collaborations <- unique(coll_tous)
  
  ### Retirer les collaborations d'étudiants avec eux-mêmes
  filter(collaborations,collaborations$etudiant1 != collaborations$etudiant2)
  
  return(list(etudiants,collaborations,cours)) # La fonction retourne une liste des 3 df
}

# ################################################################################
# #### 03.Fonctions création et injection de la bd #########################################
# ################################################################################

### Fonction pour créer la database, ses 3 tables et injecter les données de cours, collaborations et étudiants

createdb.fun <- function(clean_data) {
  dbPath <- "db/reseau.db" # Créer le chemin du fichier db
  # Création de la db
  if(file.exists(dbPath)) {
    gc() # S'assurer que la connexion au fichier .db est fermée avant de supprimer le fichier 
    file.remove(dbPath, recursive=TRUE) # Supprimer le fichier db s'il existe déjà
  }

  con <- dbConnect(SQLite(), dbname=dbPath) # Créer le fichier et la connexion à la db
  
  dbSendQuery(con,
               "CREATE TABLE etudiants (
     ID	VARCHAR(50) NOT NULL,
     region_administrative	VARCHAR(30),
     regime_coop BOOLEAN,
     formation_prealable VARCHAR(20),
     annee_debut VARCHAR(5),
     programme VARCHAR(8),
     PRIMARY KEY (ID)
   );")
  
   dbSendQuery(con,
               "CREATE TABLE collaborations (
     etudiant1 VARCHAR(50) NOT NULL,
     etudiant2 VARCHAR(50) NOT NULL,
     session VARCHAR(5),
     sigle VARCHAR(6) NOT NULL,
     PRIMARY KEY (etudiant1, etudiant2, sigle),
     FOREIGN KEY (etudiant1) REFERENCES etudiants(ID),
     FOREIGN KEY (etudiant2) REFERENCES etudiants(ID),
     FOREIGN KEY (sigle) REFERENCES cours(sigle)
   );")
  
   dbSendQuery(con,
               "CREATE TABLE cours (
     sigle VARCHAR(6) NOT NULL,
     credits INT,
     PRIMARY KEY (sigle)
   );")
  
  ### INJECT DATABASE
  dbWriteTable(con, append = TRUE, name = "collaborations", value = clean_data[[2]])
  dbWriteTable(con, append = TRUE, name = "etudiants", value = clean_data[[1]])
  dbWriteTable(con, append = TRUE, name = "cours", value = clean_data[[3]])
  
  dbDisconnect(con)
  return(dbPath)
}

# ################################################################################
# #### Fonction création df des arcs avec poids ##################################
# ################################################################################

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

# ################################################################################
# #### Fonction création matrice d'adjacence avec poids ##########################
# ################################################################################

matrice.fun <- function (arcs) {
  ### Calculer la matrice d'adjacence avec poids, où le poids représente
  # le nombre de fois que deux étudiants ont travaillé ensemble
  adjPoids <- list2dist(arcs) %>% 
    as.matrix %>% replace(is.na(.),0)
  adjPoids %>% isSymmetric # La matrice est symétrique! 
  return(adjPoids)
}

# ################################################################################
# #### Fonction création df du cci pour chaque etudiants #########################
# ################################################################################

cci.fun <- function (matrice_adj) {
  # Calculer le CCi pour tous les noeuds
  cci <- clustcoeff(matrice_adj, weighted=TRUE) %$% CCi %>% 
    data.frame(cci = .) %>% 
    rownames_to_column("ID")
  return(cci)
}

# ##############################################################################
# #### Fonction création du df des noeuds (tous les étudiants) avec cci et #####
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
# #### Fonction sélection des noeuds de la classe du df des noeuds #############
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
# #### Fonction création des statistiques descriptives du réseau ###############
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
# #### Fonction création de l'object network utilisé pour le graphique du réseau #######
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
# #### Fonction création des df de noeuds et d'arcs utilisés ###################
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



