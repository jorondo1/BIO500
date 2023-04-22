# BIO500 – Travail final

Jonathan Rondeau-Leclaire

Amélie Harbeck-Bastien

Samuel Fortin

## Résumé
Pipeline créé dans le cadre du cours BIO500 pour analyser la structure du réseau de collaboration qui unit les personnes étudiantes de la classe. À l'aide du package targets, nous avons créé un pipeline intégré nous permettant de produire une étude complète à partir de l'importation des données brutes jusqu'à la création d'un rapport prêt à publier.

## Structure du pipeline
Le répertoire github du projet est disponible [ici](https://github.com/jorondo1/BIO500). 
Spécifiquement, le code effectue les opérations suivantes en assurant la dépendance de chaque étape sur la sortie de l'étape précédente:

1. Importer et nettoyer les données brutes
2. Créer et injecter une base de données relationnelle
3. Analyser les caractéristiques du réseau
4. Produire les tables et figures pertinentes
5. Produire un rapport formaté

## Structure du répertoire
La structure du répertoire est la suivante : 
- Dossier _targets : contient les objects targets
- Dossier data : contient les données brutes en csv
- Dossier db : contient le fichier de la base de donnée créée à partie des données brutes
- Dossier rapport : contient le fichier .Rmd du rapport, le fichier de bibliographie .bib et le fichier .css pour le style du fichier .Rmd
- Dossier scripts : contient le fichier create_db.R comportant les fonctions pour les étapes 1 et 2 ainsi que le fichier analyses.R comportant les fonctions pour l'étape 3 et une partie de l'étape 4. 
- Fichier .gitignore
- Fichier _targets.R : contient le pipeline orchestrant l'exécution de toutes ces étapes
- Fichier README.md

## Exécution du programme
À partir du répertoire principal, dans R, exécuter:
```
source("_targets.R")
tar_make()
```
