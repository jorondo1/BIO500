library(pacman)
p_load(dplyr, RSQLite, magrittr, stringr, purrr, readr, tidyr, data.table, Hmisc)

##########################···
#### Contenu du script ####···
##########################···

# 1. Nettoyage et assemblage des données
#     Collaborations
#     Etudiants
#     Cours


###############################################···
#### 1. Nettoyage et assemblage des données ####···
###############################################···

# Liste des données
pathCours <- Sys.glob("donnees_BIO500/*cours*csv")
pathCollab <- Sys.glob("donnees_BIO500/*collaboration*csv")
pathEtudiant <- Sys.glob("donnees_BIO500/*etudiant*csv")

# Fonction de lecture des fichiers
lire.csv <- function(path) {
  myList <- list() # Liste vide à populer
  for (file in path) { # boucle sur chaque fichier 
    # deux types de séparateurs existent:
    sep <- ifelse(grepl(';',readLines(file, n=1)), ';', ',') 
    # lire le fichier et ajouter le df à la liste:
    myList[[which(path==file)]] <- read.csv(file, sep=sep) 
  }
  return(myList)
}

### COLLABORATIONS ############################################################

### Boucle pour importer les fichiers de collaborations de toutes les équipes et 
listCollab <- lire.csv(pathCollab)

head(listCollab[[1]]) #Vérifier que le premier df de données collab a le bon nombre de colonnes. Oui
for (i in 1: length(listCollab)) {
  if(ncol(listCollab[[i]])!=ncol(listCollab[[1]])) { # Déterminer si les df de données collab suivants ont le bon nbr de colonnes
    print(i) #Imprimer le numéro des df ne respectant pas le bon nbr de colonnes
  }
}

# le 7e df n'a pas le bon nombre, donc on l'arrange
listCollab[[7]] # Il semble avoir 5 colonnes vides supplémentaires dans le 7e df 
unique(listCollab[[7]][,5:9]) # Ça confirme que ces colonnes supplémentaires ne contiennent aucune données. 
listCollab[[7]] <- listCollab[[7]][,1:4] # Retirer ces colonnes contenant aucune information

colnames(listCollab[[1]]) # Vérifier que le premier df de données collab a les bons noms de colonnes
for (i in 1:length(listCollab)) {
  if (sum (
    listCollab[[i]] %>%
    colnames == colnames(listCollab[[1]]))!=4) {
    print(i)
  }
} # Tous les df affichent les mêmes noms de colonnes

### Réunir tous les df de données de collaborations dans un seul df
collaborations <- as.data.frame(rbindlist(listCollab, use.names=TRUE))

### Vérifier les duplicats
sum(duplicated(collaborations)) #Il y a 1996 duplicats
collaborations <- unique(collaborations) # Retirer les duplicats des données

### ETUDIANTS #################################################################

listEtudiant <- lire.csv(pathEtudiant)

# Liste des variables recherchées
colEtudiant <- c("prenom_nom","prenom","nom","region_administrative",
                 "regime_coop","formation_prealable","annee_debut","programme")

# Corriger les noms de colonne aberrants (contenant un '.')
listEtudiant %<>% # ceci est un assignment pipe, voir https://magrittr.tidyverse.org/reference/compound.html
  lapply(function(x) setNames(x, sub("\\.","",names(x)))) %>% 
  # et retirer les colonnes vides ou inutiles
  lapply(function(x) x[(names(x) %in% colEtudiant)])

etudiants_all <- rbindlist(listEtudiant) # colliger la liste en un seul df
etudiants_all[etudiants_all==""] <- NA # assigner NA aux cellules vides
etudiants_all %<>% drop_na(prenom_nom) # enlever les rangées sans identifiant

# Création d'une matrice de dissimilarité pour comparer tous les noms ensemble
# et identifier les plus similaires grâce à la distance Damerau Levenshtein
# pour trouver les fautes de frappe

etudiant_u <- etudiants_all$prenom_nom %>% unique # liste unique de tous les étudiants
dist <- etudiant_u %>% stringdistmatrix(.,.,method = "dl") # matrice de distance

# Extraire la liste des noms 
prob <- etudiant_u[which(dist<5 & dist > 0, arr.ind=TRUE)[,1]] %>% sort %>% as.matrix

# Correction à la main youpiii!
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[1:4]] <- "amelie_harbeck-bastien"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[7:8]] <- "ariane_barrette"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[11:12]] <- "cassandra_godin"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[13:14]] <- "edouard_nadon-beaumier"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[15:16]] <- "francis_boily"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[19:20]] <- "ihuoma_elsie_ebere"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[21:22]] <- "jonathan_rondeau-leclaire"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[23:24]] <- "juliette_meilleur"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[25:26]] <- "kayla_trempe-kay"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[19:20]] <- "louis-philippe_theriault"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[31:32]] <- "mael_guerin"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[33:34]] <- "marie_bughin"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[35:36]] <- "marie-christine_arseneau"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[37:38]] <- "mia_carriere"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[39:40]] <- "penelope_robert"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[41:42]] <- "philippe_barrette"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[43:44]] <- "philippe_bourassa"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[45:46]] <- "sabrina_leclercq"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[47:48]] <- "samuel_fortin"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[49:50]] <- "sara-jade_lamontagne"
etudiants_all$prenom_nom[etudiants_all$prenom_nom %in% prob[51:56]] <- "yanick_sageau"
etudiants_all$prenom_nom[etudiants_all$prenom_nom == "roxanne_bernier\t\t\t\t\t\t\t"] <- "roxanne_bernier"
etudiants_all$prenom_nom[etudiants_all$prenom_nom == "eve\xa0_dandonneau"] <- "eve_dandonneau"

### On décide d'enlever les colonnes nom et prénom car elles sont loin d'être
### essentielles et contiennent potentiellement aussi des erreurs.

etudiants_all %<>% select(-c(nom, prenom)) 

etudiants <- etudiants_all %>% 
  mutate(countNA = rowSums(is.na(.))) %>% # somme des données NA par rangée
  group_by(prenom_nom) %>% # groupement pour la prochaine étape
  filter(countNA == min(countNA)) %>% # garder les rangées avec le plus de données
  distinct(prenom_nom, # retirer les entrées dupliquées; 
           .keep_all=TRUE) %>% # les ambiguités sont sélectionnées arbitrairement
  # Traduire tous les booléens en anglais:
  mutate(regime_coop = case_when( 
    regime_coop =="VRAI" ~ "TRUE",
    regime_coop == "FAUX" ~ "FALSE"))

etudiants$region_administrative %>% unique # Certaines régions administratives sont mal écrites. 

# Régions administratives du Québec:
regAd <- c("monteregie", "saguenay-lac-saint-jean", "mauricie", "lanaudiere",
           "estrie", "outaouais", "centre-du-quebec", "bas-saint-laurent",
           "gaspesie_iles_de_la_madeleine", "montreal", "laurentides",
           "abitibi-temiscamingue", "laval", "cote_nord", "nord_du_quebec",
           "chaudiere_appalaches", "capitale-nationale")

# Boucle permettant de remplacer des erreurs de frappe mineurs grâce au fuzzy match
etu_corr <- etudiants
for (i in regAd) {
  toReplace <- agrep(i,etudiants$region_administrative) # liste d'indices avec match fuzzy
  etu_corr[toReplace,"region_administrative"] <- i # remplacer avec la bonne valeur
}

# Vérifier la valeur originale des données corrigées
etudiants[which(etudiants$region_administrative != etu_corr$region_administrative),
           "region_administrative"] # c'était bel et bien une erreur!

etudiants <- etu_corr

### COURS #####################################################################

listCours <- lire.csv(pathCours)

head(listCours[[1]]) #Vérifier que le premier df de données cours a le bon nombre de colonnes. Oui
for (i in 1: length(listCours)) {
  if(ncol(listCours[[i]])!=ncol(listCours[[1]])) { # Déterminer si les df de données collab suivants ont le bon nbr de colonnes
    print(i) #Imprimer le numéro des df ne respectant pas le bon nbr de colonnes
  }
}

# Régler le nbr de colonnes du 5e df cours
head(listCours[[5]]) # Il semble y avoir une 4e colonne vide supplémentaire dans le df 
unique(listCours[[5]][,4]) # Ça confirme que cette colonne supplémentaire ne contient aucune donnée. 
listCours[[5]] <- listCours[[5]][,1:3] # Retirer cette colonne contenant aucune information
# Régler le nbr de colonnes du 7e df cours
head(listCours[[7]]) # Il semble y avoir 6 colonnes vides supplémentaires dans le df 
unique(listCours[[7]][,4:9]) # Ça confirme que ces colonnes supplémentaires ne contiennent aucune donnée. 
listCours[[7]] <- listCours[[7]][,1:3] # Retirer ces colonnes contenant aucune information

colnames(listCours[[1]]) # Vérifier que le premier df de données cours a les bons noms de colonnes
for (i in 1:length(listCours)) {
  if (sum (
    listCours[[i]] %>%
    colnames == colnames(listCours[[1]]))!=3) {
    print(i)
  }
} # Le 4e df cours n'a pas les mêmes noms de colonnes

# Régler le nom des colonnes du 4e df cours
head(listCours[[4]]) # un i tréma s'est inséré dans le nom de la première colonne
colnames(listCours[[4]]) <- c("sigle","optionnel","credits") # Appliquer les bons noms de colonnes au 4e df cours

### Joindre tous df cours de toutes les équipes en un un seul df
cours <- as.data.frame(rbindlist(listCours, use.names=TRUE))

### Retirer la colonne 'optionnel' car le même cours peut être optionnel pour 
# certains étudiants et non optionnel pour d'autres, selon leur programme
cours <- cours[,c("sigle","credits")]

### Retirer les rangées du df cours qui ne contiennent pas de données dans la colonne 'sigle'
cours <- cours[!cours$sigle %in% c("",NA),]

### Trouver les sigles de cours n'existant pas en comparant à la liste de 
# cours officielle du bac en Biologie de l'UdeS
cours$sigle[order(cours$sigle)] #les cours TSB302 et BIO400 n'existent pas 

### Corriger les sigle erronés à la fois pour le df cours et le df collaborations
cours$sigle[cours$sigle == "TSB302"] <- "TSB303" # Modifier TSB302 en TSB303 pour df cours
collaborations$sigle[collaborations$sigle == "TSB302"] <- "TSB303" # Modifier TSB302 en TSB303 pour df collaborations
cours$sigle[cours$sigle == "BIO400"] <- "BOT400" # Modifier BIO400 en BOT400 pour df cours
collaborations$sigle[collaborations$sigle == "BIO400"] <- "BOT400" # Modifier BIO400 en BOT400 pour df collaborations

### Trouver les sigles dupliqués avec des crédits différents
nrow(unique(cours))-length(unique(cours$sigle)) #Calculer le nbr d'erreurs de crédits dans les données
unique(cours)[duplicated(unique(cours)[,1]),1] # Identifier les cours affichant plusieurs options de crédits

### Modifier les valeurs de crédits erronnées pour les sigles dupliqués trouvés
cours$credits[cours$sigle == "BIO109"] <- 1 
cours$credits[cours$sigle == "ECL515"] <- 2
cours$credits[cours$sigle == "TSB303"] <- 2
cours$credits[cours$sigle == "BOT400"] <- 1

### Valider que les sigles dupliqués avec des crédits différents ont été corrigés
nrow(unique(cours))-length(unique(cours$sigle))


##########################################···
#### Cohérence des variables partagées ####···
##########################################···

### Cohérence collaboration - cours ############################################

# Trouver les sigles présents dans cours, mais pas dans collaborations:
(orphelins <- unique(cours$sigle)[unique(cours$sigle) %nin% 
                         unique(collaborations$sigle)])
  # Il y a 10 cours présents dans cours qui n'ont pas donné lieu a des collaborations

# Retirer les cours présents dans cours mais pas dans collaborations
# car ces cours ne présentaient pas de collaborations:
cours <- cours[cours$sigle %nin% orphelins,] 

# Trouver les sigles présents dans collaborations absents de cours
unique(collaborations$sigle)[unique(collaborations$sigle) %nin% unique(cours$sigle)]
  # 4 erreurs sont présentes dans les sigles de collaborations

# Retirer les collaborations relatives à GBI105... il n'y avait pas de collaborations dans ce cours:
collaborations <- collaborations[!collaborations$sigle == "GBI105",]
# Retirer les collaborations sans sigle:
collaborations <- collaborations[!collaborations$sigle == "",] 
# Modifier le sigle GAE500 par GAE550:
collaborations$sigle[collaborations$sigle == "GAE500"] <- "GAE550" 
# Modifier le sigle ECL405 pour ECL404
collaborations$sigle[collaborations$sigle == "ECL405"] <- "ECL404" 

### Cohérence collaboration - étudiants ########################################

enleve <- setdiff(etudiants$prenom_nom, collaborations$etudiant1 %>% unique)
# un des étudiants n'est jamais mentionné dans les collaborations, on le 
# retire donc de la table etudiants :
etudiants %<>% filter(prenom_nom %nin% enleve)

# Vérifier la différence entre (etudiant1 ou etudiant2) et prenom_nom
setdiff(collaborations$etudiant1 %>% unique, etudiants$prenom_nom)
setdiff(collaborations$etudiant2 %>% unique, etudiants$prenom_nom)
# Il y a 38 noms qui sont soit des erreurs de frappe, soit carrément absents 
# de la table etudiants.

# On corrige quelques erreurs à la mitaine, où la présence d'un backslash
# nous donne du fil à retordre:
coll_corr <- collaborations
coll_corr$etudiant1[coll_corr$etudiant1=="juliette_meilleur\xa0"] <- "juliette_meilleur"
coll_corr$etudiant2[coll_corr$etudiant2=="juliette_meilleur\xa0"] <- "juliette_meilleur"
coll_corr$etudiant1[coll_corr$etudiant1=="mia_carriere\xa0"] <- "mia_carriere"  
coll_corr$etudiant2[coll_corr$etudiant2=="mia_carriere\xa0"] <- "mia_carriere"  
coll_corr$etudiant1[coll_corr$etudiant1=="eve\xa0_dandonneau"] <- "eve_dandonneau"  
coll_corr$etudiant2[coll_corr$etudiant2=="eve\xa0_dandonneau"] <- "eve_dandonneau"  

# On remplace les autres erreurs avec un fuzzy match :
id <- etudiants$prenom_nom
for (i in id) {
  toReplace <- agrepl(i,coll_corr$etudiant1) # liste d'indices avec match fuzzy
  coll_corr[toReplace,"etudiant_1"] <- i # remplacer avec la bonne valeur
}
for (i in id) {
  toReplace <- agrepl(i,coll_corr$etudiant2)
  coll_corr[toReplace,"etudiant_2"] <- i 
  }

# ultimate sanity check
setdiff(setdiff(coll_corr$etudiant1 %>% unique, etudiants$prenom_nom),
        setdiff(coll_corr$etudiant2 %>% unique, etudiants$prenom_nom))


#######################################···
#### Création et injection de la bd ####···
#######################################···

dbPath <- "db/travail_final.db"
# Création de la db
if(file.exists(dbPath)) {
  file.remove(dbPath)
  con <- dbConnect(SQLite(), dbname=dbPath)
  }

# Création des tables brutes et suppression des dupliqués
collab_table <- read_tsv('db/collaborations.txt') %>% distinct %>% 
  # retirer les espaces début/fin de string
  mutate(across(where(is.character), str_squish))

etudiant_table <- read_tsv('db/etudiants.txt') %>% distinct %>% 
  mutate(across(where(is.character), str_squish)) %>% 
  # création d'un etudiant_ID de format E000
  mutate(etudiant_ID = paste0("E", str_pad(row_number(),3,"left","0")))

cours_table <- read_tsv('db/cours.txt') %>% distinct %>% 
  mutate(across(where(is.character),str_squish))

dbSendQuery(con, 
"CREATE TABLE etudiants (
  etudiant_ID VARCHAR(4) NOT NULL,
  prenom_nom	VARCHAR(30) NOT NULL,
  prenom	VARCHAR(15),
  nom	 VARCHAR(15), 
  region_administrative	VARCHAR(30),
  regime_coop BOOLEAN,
  formation_prealable VARCHAR(20),
  annee_debut VARCHAR(5), 
  programme VARCHAR(8),
  PRIMARY KEY (etudiant_ID)
);")

dbSendQuery(con, 
"CREATE TABLE collab (
  etudiant_1 VARCHAR(30) NOT NULL,
  etudiant_2 VARCHAR(30) NOT NULL,
  session VARCHAR(3),
  sigle VARCHAR(6) NOT NULL,
  PRIMARY KEY (etudiant_1, etudiant_2, sigle),
  FOREIGN KEY (etudiant_1) REFERENCES etudiants(etudiant_ID),
  FOREIGN KEY (etudiant_2) REFERENCES etudiants(etudiant_ID),
  FOREIGN KEY (sigle) REFERENCES cours(sigle)
);")

dbSendQuery(con, 
"CREATE TABLE cours (
  sigle VARCHAR(6) NOT NULL,
  optionnel BOOLEAN,
  credits INT(2),
  PRIMARY KEY (sigle)
);")

### INJECT DATABASE
dbWriteTable(con, append = TRUE, name = "collab", value = collab_table)
dbWriteTable(con, append = TRUE, name = "etudiants", value = etudiant_table)
dbWriteTable(con, append = TRUE, name = "cours", value = cours_table)

# Nombre de liens par étudiant?
# Décmopte de liens par paire d'étudiants
# Enregistrer le tout en CSV
# puis dans R,
# calculer nombre d'étudiants, nombre de liens, et connectance du réseau
# calculer le nombre de liens moyens par étudiant et la variance
