######################################################
# Script pour nettoyer et assembler les données
# Victor Cameron
# 15 mars 2023
######################################################


######################################################
## Etapes (*À ADAPTER*)
# 1. Charger tous les donnees provenants du dossier data/raw
# 2. Pour chaque table (etudiant, cours, collaborations):
# 	- Vérifier si les noms de colonnes sont standardisés
# 	- Vérifier si chacune des valeurs pour chaque colonne respecte le formatage
# 	- Réparer les colonnes que ne respectent pas le format (travail manuel ici)
# 	- Autres vérifications dans et entre les groupes
# 	- Fusionner les donnees de chaque groupe en un seul data.frame
# 3. Sauvegarder les données fusionnées de chaque table dans le dossier data/clean
######################################################


#-----------------------------------------------------
# 1. Charger les données
#
# Assumant que les données sont sauvées dans le 
# sous-répertoire data/raw
#-----------------------------------------------------
# Extraire le nom des fichers de chaque groupe
allFiles <- dir('donnees_BIO500/')

# Tables à fusioner
tabNames <- c('collaboration', 'cour', 'etudiant')

# Nombre de groupes
nbGroupe <- length(grep(tabNames[1], allFiles))

# Charger les donnees
for(tab in tabNames) {
    # prendre seulement les fichers de la table specifique `tab`
    tabFiles <- allFiles[grep(tab, allFiles)]
    
    for(groupe in 1:nbGroupe) {
        # Definir le nom de l'obj dans lequel sauver les donnees de la table `tab` du groupe `groupe`
        tabName <- paste0(tab, "_", groupe)

        # Avant  de charger les données, il faut savoir c'est quoi le séparateur utilisé car
        # il y a eu des données separées par "," et des autres separes par ";"
        ficher <- paste0('donnees_BIO500/', tabFiles[groupe])
        L <- readLines(ficher, n = 1) # charger première ligne du donnée
        separateur <- ifelse(grepl(';', L), ';', ',') # S'il y a un ";", separateur est donc ";"

        # charger le donnée avec le bon séparateur et donner le nom `tabName`
        assign(tabName, read.csv(ficher, sep = separateur, stringsAsFactors = FALSE))

    }
}

# nettoyer des objets temporaires utilisé dans la boucle
rm(list = c('allFiles', 'tab', 'tabFiles', 'tabName', 'ficher', 'groupe'))


#-----------------------------------------------------
# 2.
#-----------------------------------------------------