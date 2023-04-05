library(pacman)
p_load(tidygraph, igraph, dplyr, GGally, network, sna, intergraph, magrittr,
       RColorBrewer, NetworkToolbox, sparsebnUtils, spaa, tibble, rstatix, ggpubr)
set.seed(04042023)
# 
# # Create edge list
# arcs <- collaborations %>% count(etudiant1,etudiant2) %>% 
#   filter(etudiant1!=etudiant2) %>% # remove self-edges
#   # remove duplicate from list
#   mutate(id = case_when(etudiant1<etudiant2 ~ paste0(etudiant1,"+",etudiant2),
#                         etudiant2<etudiant1 ~ paste0(etudiant2,"+",etudiant1))) %>% 
#   distinct(id, .keep_all=TRUE) %>% mutate_if(is.character, as.factor)
# 
# adjPoids <- list2dist(arcs) %>% as.matrix  %>% replace(is.na(.),0)

# Préparer la matrice d'arcs (1 arc = 1 collaboration) avec poids
arcs <- collaborations %>% count(etudiant1,etudiant2) %>% 
  filter(etudiant1!=etudiant2) %>% mutate_if(is.character, as.factor)

# Matrice d'adjecence avec poids
adjPoids <- list2dist(arcs) %>% as.matrix %>% replace(is.na(.),0)
adjPoids %>% isSymmetric # La matrice est symétrique!

# # Matrice sans poids avec seulement les étudiants du cours
# adjPA <- arcs[c(arcs$etudiant1 %in% etudiants$ID,
#                 arcs$etudiant2 %in% etudiants$ID),] %>%
#   list2dist %>% as.matrix %>% replace(.!=0,1)
### Connectivité du réseau
### Calculé seulement pour les étudiants du cours BIO500
# conn.etu <- conn(adjPA)

# ggplot(as.data.frame(arcs), aes(x=etudiant1,y=etudiant2,fill=n)) + geom_tile() +
#   ylim(rev(levels(arcs$etudiant2))) + xlim(levels(arcs$etudiant1))

# hist(degree.distribution(g))
# layout <- layout_arcsith_kk(g)
# plot(g, layout=layout)
# summary(g)

### COMPUTE CCi pour tous les noeuds
cci <- clustcoeff(adjPoids, weighted=TRUE) %$% CCi %>% 
  data.frame(cci = .) %>% rownames_to_column("ID")

### Création d'une table de données pour tester les hypothèses
cciGroup <- etudiants %>% 
  dplyr::select(ID, formation_prealable, regime_coop, annee_debut) %>% 
  left_join(cci, by="ID") %>%  # add clustering coefficient
  # Conserver seulement les étudiants ayant au moins une des trois variables d'intérêt:
  dplyr::filter(!is.na(formation_prealable) || 
                  !is.na(annee_debut) ||
                  !is.na(regime_coop)) %>% 
  mutate(yr = case_when(annee_debut == "A2020" ~ "A2020",
                        TRUE ~ "Autre")) %>% 
  mutate(coop = case_when(regime_coop == "TRUE" ~ "COOP",
                          regime_coop == "FALSE" ~ "Régulier",
                          TRUE ~ NA)) %>% 
  mutate_if(is.character,as.factor) %>% ungroup

### NORMALITY TEST
# H0: la variable cci suit une distribution normale
cciGroup %>% group_by(formation_prealable) %>% 
  filter(!is.na(formation_prealable)) %>% 
  shapiro_test(cci)

cciGroup %>% group_by(yr) %>% 
  shapiro_test(cci)

cciGroup %>% group_by(regime_coop) %>% 
  shapiro_test(cci) # le cci est normalement distribuée à travers tous les groupes!

### KRUSKAL-WALLIS TEST
# H0: tous les groupes ont la même distribution de cci

(form.stat <- cciGroup %>% 
  dunn_test(cci ~ formation_prealable)) # NS

(yr.stat <- cciGroup %>%
  dunn_test(cci ~ yr)) # SIGNIFICATIF

(reg.stat <- cciGroup %>%
  dunn_test(cci ~ coop)) # NS


### PLOTS
ggplot(cciGroup, aes(y = cci)) + 
  geom_histogram(binwidth = 0.1, center=1) +
  coord_flip() + 
  theme_minimal()

# CCi par formation préalable
(cciForm.plot <- cciGroup %>% filter(!is.na(formation_prealable)) %>% 
  ggplot(aes(x = formation_prealable, y = cci)) +
  geom_boxplot(aes(fill = formation_prealable)) + geom_point(position="jitter") +
  theme_light() +
  theme(legend.position="none")+ # Hide legend
  stat_pvalue_manual(form.stat,  
                    label = "p.adj.signif", 
                    tip.length = 0.01, 
                    y.position = 2.2, 
                    step.increase = 0.06) + # Add stat test
  labs(x = "Formation préalable", y = "Coefficient de regroupement",
       title = "Distribution des coefficients de regroupement individuels par formation préalable.")
  )

# CCi par programme (coop vs régulier)
(cciReg.plot <- cciGroup %>% filter(!is.na(coop)) %>% 
  ggplot(aes(x = coop, y = cci)) +
  geom_boxplot(aes(fill = coop)) + geom_point(position="jitter") +
  theme_light() +
  theme(legend.position="none")+ # Hide legend
  stat_pvalue_manual(reg.stat,  
                     label = "p.adj.signif",
                     y.position = 2.2) + # Add stat test
  labs(x = "Régime Coop", y = "Coefficient de regroupement",
       title = "Distribution des coefficients de regroupement individuels par type de programme.")
)

# CCi par session de début de bac
(cciYr.plot <- cciGroup %>% filter(!is.na(yr)) %>% 
  ggplot(aes(x = yr, y = cci)) +
  geom_boxplot(aes(fill = yr)) + geom_point(position="jitter") +
  theme_light() +
    theme(legend.position="none")+ # Hide legend
    stat_pvalue_manual(yr.stat,  
                       label = "p.adj.signif",
                       y.position = 2.2) + # Add stat test
    labs(x = "Première session", y = "Coefficient de regroupement",
         title = "Distribution des coefficients de regroupement individuels par session de début de bac.")
)

# Distribution de la densité
mean(arcs$n) # en moyenne 1.95 interactions par personne
sd(arcs$n) # ±2.50 

L <- nrow(arcs)/2 # /2 parce que tout est (théoriquement) doublé
S <- nrow(etudiants)
densite <- L/S
m <-  S*(S-1)/2 # nombre d'interactions possibles dans un réseau unipartite simple non-dirigé
(C0 <- L/m) # environ 5.7 %; sous-estimée, car les interactions entre étudiants hors-cours ne sont pas toutes comptabilisées

# Calculer le nombre d'arcs par personne
k <- collaborations %>% count(etudiant1,etudiant2) %>%
  filter(etudiant1!=etudiant2) %>% # remove self-edges
  # remove duplicate from list
  mutate(id = case_when(etudiant1<etudiant2 ~ paste0(etudiant1,"+",etudiant2),
                        etudiant2<etudiant1 ~ paste0(etudiant2,"+",etudiant1))) %>%
  distinct(id, .keep_all=TRUE) %>% mutate_if(is.character, as.factor) %>% 
  group_by(etudiant1) %>% # varie légèrement si on utilise etudiant2, erreurs de saisie (devrait être symétrique)
  summarise(n=sum(n))

# Distribution de la densité
ggplot(k, aes(y = n)) + 
  geom_histogram(bins = length(unique(k$n)),
                 center=1) +
  coord_flip() + 
  theme_minimal()

shapiro_test(k,n) # très pas normale

# Create weighted undirected graph using igraph
g <- graph_from_adjacency_matrix(adjPoids, 
                                 mode = 'undirected',
                                 weighted = TRUE) %>% 
  simplify

g %>% diameter(directed = FALSE) # 13

# Modularité/communautés (subgraphs)
(fc <- cluster_fast_greedy(g) %>% membership %>% as.character)
comm <- etudiants
comm[,"commID"] <- fc

### NETarcsORK PLOTS

## using ggnet, plot by regime
net = network(as.matrix(arcs[,1:2]), directed = FALSE) 
net %v% "comm" <- arrange(comm, by = ID) %$% commID

ggnet2(net, size=6, weight=arcs$n, color="comm", palette="Set2")

net %v% "regime" = arrange(comm, by = ID) %$% regime_coop

ggnet2(net, size=6, weight = arcs$n, color="regime", palette = "Set2")

## plot by formation prealable 
net %v% "form" = arrange(comm, by = ID) %$% formation_prealable
ggnet2(net, size=6, weight = arcs$n, color="form", palette = "Set2")

net %v% "start" = arrange(comm, by = ID) %$% annee_debut
net %v% "prog" = arrange(comm, by = ID) %$% programme




## arcs with labels
ggnet2(net, label = comm$ID %>% sort, label.size = 3,
       color = "form", palette = "Set3", segment.alpha=0.2,
       segment.color = "grey90", size="degree", size.cut=TRUE,
       segment.size = 0.1)


data.frame(start = comm$annee_debut) %>% 
  filter(!is.na(start)) %>% 
  mutate(start=case_when(start=="A2020" ~ "A2020",
                         TRUE ~ "Autre")) %>% 
  group_by(start) %>% count

