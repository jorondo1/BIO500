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
colCollab <- c("etudiant1","etudiant2","sigle","session")

# Retirer les caractères weird
listCollab %<>% lapply(function(df) {
  # conserver seulement les colonnes d'intérêt
  df <- df[, colCollab]
  # remove "<a0>" from all columns
  df[] <- lapply(df, function(col) gsub("<a0>", "", col))
  return(df)
})

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

# assigner NA aux cellules vides:
etu_tous[etu_tous==""] <- NA
# enlever les rangées sans identifiant:
etu_tous %<>% drop_na(ID) 

# Création d'une matrice de dissimilarité pour comparer tous les noms ensemble
# et identifier les plus similaires grâce à la distance Damerau-Levenshtein
# pour trouver les fautes de frappe.

etudiant_u <- etu_tous$ID %>% unique # liste unique de tous les étudiants
dist <- etudiant_u %>% stringdistmatrix(.,.,method = "dl") # matrice de distance

# Extraire la liste des noms 
(prob <- etudiant_u[which(dist<5 & dist > 0, arr.ind=TRUE)[,1]] %>% sort %>% as.matrix)
# Attention, certains noms sont simplement similaires et 
# n'ont pas besoin d'être remplacés!

# On crée une liste des noms mal épelés
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

# Enlever le i trémas que certains voient (mais pas sur mon mac!)
names(listCours[[4]]) <- c("sigle","optionnel", "credits")

### Retirer la colonne 'optionnel' car le même cours peut être optionnel pour 
# certains étudiants et non optionnel pour d'autres, selon leur programme
colCours <- c("sigle", "credits")
listCours %<>% lapply(function(x) x[(names(x) %in% colCours)])

### Joindre tous df cours de toutes les équipes en un un seul df
cours <- as.data.frame(rbindlist(listCours, use.names=TRUE))

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
nrow(unique(cours))-length(unique(cours$sigle)) # 0 = all good!

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

# L'équipe 4 a rapporté une erreur dans 6 de ses entrées, qu'on corrige manuellement
# parce que ce sont les seules entrées où ces données sont manquantes.
coll_corr[coll_corr$sigle == "",3:4] <- c("ECL615","E2022")

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

# Les autres cas sont absents de etu_corr, on les ajoute:
manquants <- data.frame(ID=setdiff(coll_corr$etudiant1 %>% unique, id))

etudiants <- rbind(etu_corr, manquants)

# Sanity check ultime :
setdiff(coll_corr$etudiant1,etudiants$ID)
setdiff(coll_corr$etudiant2,etudiants$ID)
setdiff(etudiants$ID,coll_corr$etudiant1)
setdiff(etudiants$ID,coll_corr$etudiant2)

# 100% des ID sont cohérents!

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
