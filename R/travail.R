library(pacman)
p_load(dplyr, RSQLite, magrittr, stringr, purrr, readr, 
       stringdist, tidyr, data.table, Hmisc)

##########################···
#### Contenu du script ####···
##########################···

# 01.Nettoyage et assemblage des données
#       Collaborations
#       Etudiants
#       Cours
# 02.Cohérence entre les variables partagées
#       Cohérence collaborations - cours
#       Cohérence collaborations - étudiants
# 03.Création et injection de la bd

################################################################################
#### 01.Nettoyage et assemblage des données ####################################
###############################################################################

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

#####################···
### COLLABORATIONS ###···
#####################···

### Importer les fichiers de collaborations de toutes les équipes
listCollab <- lire.csv(pathCollab)

# Vérifier que le premier fichier collab a le bon nombre de colonnes
head(listCollab[[1]]) # oui !

# Déterminer si les df de données collab suivants ont le bon nbr de colonnes
for (i in 1: length(listCollab)) {
  if(ncol(listCollab[[i]])!=ncol(listCollab[[1]])) { 
    print(i) #Imprimer le numéro des df ne respectant pas le bon nbr de colonnes
  }
}

# le 7e df n'a pas le bon nombre, on l'arrange :
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
coll_tous <- as.data.frame(rbindlist(listCollab, use.names=TRUE))

### Vérifier les duplicats
sum(duplicated(coll_tous)) #Il y a 1996 duplicats
coll_corr <- coll_tous %>% unique #Retirer les duplicats des données

#####################···
### ETUDIANTS ########···
#####################···

listEtudiant <- lire.csv(pathEtudiant)

# Liste des variables recherchées
colEtudiant <- c("prenom_nom","prenom","nom","region_administrative",
                 "regime_coop","formation_prealable","annee_debut","programme")

# Corriger les noms de colonne aberrants (contenant un '.')
listEtudiant %<>% 
  lapply(function(x) setNames(x, sub("\\.","",names(x)))) %>% 
  # et retirer les colonnes vides ou inutiles
  lapply(function(x) x[(names(x) %in% colEtudiant)])

# colliger la liste en un seul df
etu_tous <- rbindlist(listEtudiant) %>%  
  # prenom_nom devient ID, car cette variable sera utilisée comme identifiant unique
  mutate(ID=prenom_nom, .keep="unused") 

# assigner NA aux cellules vides:
etu_tous[etu_tous==""] <- NA
# enlever les rangées sans identifiant:
etu_tous %<>% drop_na(ID) 

# Création d'une matrice de dissimilarité pour comparer tous les noms ensemble
# et identifier les plus similaires grâce à la distance Damerau Levenshtein
# pour trouver les fautes de frappe

etudiant_u <- etu_tous$ID %>% unique # liste unique de tous les étudiants
dist <- etudiant_u %>% stringdistmatrix(.,.,method = "dl") # matrice de distance

# Extraire la liste des noms 
prob <- etudiant_u[which(dist<5 & dist > 0, arr.ind=TRUE)[,1]] %>% sort %>% as.matrix

# Correction à la main, youpiii!
etu_tous$ID[etu_tous$ID %in% prob[1:4]] <- "amelie_harbeck-bastien"
etu_tous$ID[etu_tous$ID %in% prob[7:8]] <- "ariane_barrette"
etu_tous$ID[etu_tous$ID %in% prob[11:12]] <- "cassandra_godin"
etu_tous$ID[etu_tous$ID %in% prob[13:14]] <- "edouard_nadon-beaumier"
etu_tous$ID[etu_tous$ID %in% prob[15:16]] <- "francis_boily"
etu_tous$ID[etu_tous$ID %in% prob[19:20]] <- "ihuoma_elsie_ebere"
etu_tous$ID[etu_tous$ID %in% prob[21:22]] <- "jonathan_rondeau-leclaire"
etu_tous$ID[etu_tous$ID %in% prob[23:24]] <- "juliette_meilleur"
etu_tous$ID[etu_tous$ID %in% prob[25:26]] <- "kayla_trempe-kay"
etu_tous$ID[etu_tous$ID %in% prob[27:30]] <- "louis-philippe_theriault"
etu_tous$ID[etu_tous$ID %in% prob[31:32]] <- "mael_guerin"
etu_tous$ID[etu_tous$ID %in% prob[33:34]] <- "marie_bughin"
etu_tous$ID[etu_tous$ID %in% prob[35:36]] <- "marie-christine_arseneau"
etu_tous$ID[etu_tous$ID %in% prob[37:38]] <- "mia_carriere"
etu_tous$ID[etu_tous$ID %in% prob[39:40]] <- "penelope_robert"
etu_tous$ID[etu_tous$ID %in% prob[41:42]] <- "philippe_barrette"
etu_tous$ID[etu_tous$ID %in% prob[43:44]] <- "philippe_bourassa"
etu_tous$ID[etu_tous$ID %in% prob[45:46]] <- "sabrina_leclercq"
etu_tous$ID[etu_tous$ID %in% prob[47:48]] <- "samuel_fortin"
etu_tous$ID[etu_tous$ID %in% prob[49:50]] <- "sara-jade_lamontagne"
etu_tous$ID[etu_tous$ID %in% prob[51:56]] <- "yanick_sageau"
etu_tous$ID[etu_tous$ID == "roxanne_bernier\t\t\t\t\t\t\t"] <- "roxanne_bernier"
etu_tous$ID[etu_tous$ID == "eve\xa0_dandonneau"] <- "eve_dandonneau"

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
    regime_coop == "FAUX" ~ "FALSE"))

etu_tous$region_administrative %>% unique 
  # Certaines régions administratives sont mal écrites!

# Régions administratives du Québec:
regAd <- c("monteregie", "saguenay-lac-saint-jean", "mauricie", "lanaudiere",
           "estrie", "outaouais", "centre-du-quebec", "bas-saint-laurent",
           "gaspesie_iles_de_la_madeleine", "montreal", "laurentides",
           "abitibi-temiscamingue", "laval", "cote_nord", "nord_du_quebec",
           "chaudiere_appalaches", "capitale-nationale")

# Boucle permettant de remplacer des erreurs de frappe mineurs grâce au fuzzy match
etu_corr <- etu_tous
for (i in regAd) {
  toReplace <- agrep(i,etu_tous$region_administrative) # liste d'indices avec match fuzzy
  etu_corr[toReplace,"region_administrative"] <- i # remplacer avec la bonne valeur
}

# Vérifier la valeur originale des données corrigées
etu_tous[which(etu_tous$region_administrative != 
                      etu_corr$region_administrative),
           "region_administrative"] # c'était bel et bien une erreur!

#####################···
### COURS ############···
#####################···

listCours <- lire.csv(pathCours)

#Vérifier que le premier df de données cours a le bon nombre de colonnes.
head(listCours[[1]])  # Oui
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

### Corriger les sigle erronés à la fois pour cours et coll_corr
cours$sigle[cours$sigle == "TSB302"] <- "TSB303" # Modifier TSB302 en TSB303 pour df cours
coll_corr$sigle[coll_corr$sigle == "TSB302"] <- "TSB303" # Modifier TSB302 en TSB303 pour df coll_corr
cours$sigle[cours$sigle == "BIO400"] <- "BOT400" # Modifier BIO400 en BOT400 pour df cours
coll_corr$sigle[coll_corr$sigle == "BIO400"] <- "BOT400" # Modifier BIO400 en BOT400 pour df coll_corr

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

################################################################################
#### 02.Cohérence des variables partagées ######################################
################################################################################

#########################################···
### Cohérence collaboration - cours ######···
#########################################···

# Trouver les sigles présents dans cours, mais pas dans coll_corr:
(orphelins <- unique(cours$sigle)[unique(cours$sigle) %nin% 
                         unique(coll_corr$sigle)])
  # Il y a 10 cours présents dans cours qui n'ont pas donné lieu a des coll_corr

# Retirer les cours présents dans cours mais pas dans coll_corr
# car ces cours ne présentaient pas de coll_corr:
cours <- cours[cours$sigle %nin% orphelins,] 

# Trouver les sigles présents dans coll_corr absents de cours
unique(coll_corr$sigle)[unique(coll_corr$sigle) %nin% unique(cours$sigle)]
  # 4 erreurs sont présentes dans les sigles de coll_corr

# Retirer les coll_corr relatives à GBI105... il n'y avait pas de coll_corr dans ce cours:
coll_corr <- coll_corr[!coll_corr$sigle == "GBI105",]
# Retirer les coll_corr sans sigle:
coll_corr <- coll_corr[!coll_corr$sigle == "",] 
# Modifier le sigle GAE500 par GAE550:
coll_corr$sigle[coll_corr$sigle == "GAE500"] <- "GAE550" 
# Modifier le sigle ECL405 pour ECL404
coll_corr$sigle[coll_corr$sigle == "ECL405"] <- "ECL404" 

###########################################···
### Cohérence collaboration - étudiants ####···
###########################################···

# Vérifier la différence entre les identifiants étudiants:
setdiff(etu_corr$ID, coll_corr$etudiant1 %>% unique)

# Vérifier la différence entre (etudiant1 ou etudiant2) et ID
setdiff(coll_corr$etudiant1 %>% unique, etu_corr$ID)
setdiff(coll_corr$etudiant2 %>% unique, etu_corr$ID)
# Plusieurs étudiants ont des erreurs de frappe!

# On remplace les autres erreurs avec un fuzzy match :
id <- etu_corr$ID
for (i in id) {
  toReplace <- agrepl(i, # ID à fuzzy-matcher
                      coll_corr$etudiant1, # liste à chercher
                      ignore.case = FALSE,
                      max.distance = 0.0001) # très sensible pour éviter de mélanger les noms similaires
  coll_corr[toReplace,"etudiant1"] <- i # remplacer avec le bon ID
}
# Une autre fois pour la colonne etudiant2
for (i in id) {
  toReplace <- agrepl(i,coll_corr$etudiant2, ignore.case = FALSE, max.distance = 0.0001)
  coll_corr[toReplace,"etudiant2"] <- i 
  }

# Ajouter les étudiants présents dans collaboration mais absents de etudiant
# à la table etudiant
setdiff(coll_corr$etudiant1, id)
setdiff(coll_corr$etudiant2, id)
setdiff(id, coll_corr$etudiant1)
setdiff(id, coll_corr$etudiant2)

# Il reste un cas problématique qu'on règle à la main:
coll_corr$etudiant1[coll_corr$etudiant1 == "eve\xa0_dandonneau"] <- "eve_dandonneau"
coll_corr$etudiant2[coll_corr$etudiant2 == "eve\xa0_dandonneau"] <- "eve_dandonneau"

# Les autres cas sont absents de etu_corr, on les ajoute:
manquants <- data.frame(ID=setdiff(coll_corr$etudiant1 %>% unique, id))

etudiants <- rbind(etu_corr, manquants)

# Sanity check ultime :
setdiff(coll_corr$etudiant1,etudiants$ID)
setdiff(coll_corr$etudiant2,etudiants$ID)
setdiff(etudiants$ID,coll_corr$etudiant1)
setdiff(etudiants$ID,coll_corr$etudiant2)

# renommer la table finale de collaborations
collaborations <- coll_corr

################################################################################
#### 03.Création et injection de la bd #########################################
################################################################################

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
  ID	VARCHAR(30) NOT NULL,
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
