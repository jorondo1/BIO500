library(pacman)
p_load(tidygraph, igraph, dplyr, GGally, network, sna, intergraph, magrittr,
       RColorBrewer, NetworkToolbox, sparsebnUtils, spaa, tibble, rstatix, ggpubr,
       visNetwork)

###!!! Muter les variabls tout au début à partir de etudiants!



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

### NORMALITY TEST
# H0: la variable cci suit une distribution normale
cciGroup %>% group_by(form) %>% 
  filter(form != "Inconnu") %>% # est-ce qu'on les retire ici ou plus loin???
  shapiro_test(cci)

cciGroup %>% group_by(annee) %>% 
  shapiro_test(cci)

cciGroup %>% group_by(regime) %>% 
  shapiro_test(cci) 

# le cci est normalement distribuée à travers tous les groupes, sauf les inconnus.
# Cela fait du sens, car les inconnus sont des gens de l'extérieur du cours et 
# leur réseau est loin d'être complet.

### KRUSKAL-WALLIS TEST
# H0: tous les groupes ont la même distribution de cci

(form.stat <- cciGroup %>% filter(form != "Inconnu") %>% 
    dunn_test(cci ~ form)) # NS

(annee.stat <- cciGroup %>% filter(annee != "Inconnu") %>% 
    dunn_test(cci ~ annee)) # SIGNIFICATIF

(reg.stat <- cciGroup %>%filter(regime != "Inconnu") %>% 
    dunn_test(cci ~ regime)) # NS

### PLOTS
ggplot(cciGroup, aes(y = cci)) + 
  geom_histogram(binwidth = 0.1, center=1) +
  coord_flip() + 
  theme_minimal()

# CCi par formation préalable
(cciForm.plot <- cciGroup %>% filter(form!="Inconnu") %>% 
  ggplot(aes(x = form, y = cci)) +
  geom_boxplot(aes(fill = form)) + geom_point(position="jitter") +
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
(cciReg.plot <- cciGroup %>% filter(regime!="Inconnu") %>% 
  ggplot(aes(x = regime, y = cci)) +
  geom_boxplot(aes(fill = regime)) + geom_point(position="jitter") +
  theme_light() +
  theme(legend.position="none")+ # Hide legend
  stat_pvalue_manual(reg.stat,  
                     label = "p.adj.signif",
                     y.position = 2.2) + # Add stat test
  labs(x = "Régime regime", y = "Coefficient de regroupement",
       title = "Distribution des coefficients de regroupement individuels par type de programme.")
)

# CCi par session de début de bac
(cciYr.plot <- cciGroup %>% filter(annee!="Inconnu") %>% 
  ggplot(aes(x = annee, y = cci)) +
  geom_boxplot(aes(fill = annee)) + geom_point(position="jitter") +
  theme_light() +
    theme(legend.position="none")+ # Hide legend
    stat_pvalue_manual(annee.stat,  
                       label = "p.adj.signif",
                       y.position = 2.2) + # Add stat test
    labs(x = "Première session", y = "Coefficient de regroupement",
         title = "Distribution des coefficients de regroupement individuels par session de début de bac.")
)

# Distribution de la densité
ggplot(k, aes(y = n)) + 
  geom_histogram(bins = length(unique(k$n)),
                 center=1) +
  coord_flip() + 
  theme_minimal() +
  labs(title = "Distribution du nombre de personnes collaboratrices par personne étudiante",
       y = "Nombre de personnes collaboratrices", x = "Fréquence")

shapiro_test(k,n) # très pas normale

# Create weighted undirected graph using igraph
g <- graph_from_adjacency_matrix(adjPoids, 
                                 mode = 'undirected',
                                 weighted = TRUE) %>% 
  simplify

g %>% diameter(directed = FALSE) # 13

# Betweenness (mesure de centralité)
ebs <- edge_betweenness(g)
as_edgelist(g)[ebs == min(ebs), ]

# Modularité/communautés (subgraphs)
# (fc <- cluster_louvain(g, resolution = 0.5) %>% membership %>% as.character)
  
comm[,"commID"] <- fc

aov(data = comm, commID ~ programme) %>% summary
###! Pas très pertinent, aucune structure n'est évidente à l'oeil 
###! et les supposées communautés ne corrèlent avec aucune variable

### NETWORK PLOTS
# Arcs avec un seul par paire (matnrice diagonale)

## using ggnet to create network plots:

# Create the network
net = network(as.matrix(arcs2[,1:2]), directed = FALSE) 

# Create a palette-generating function
colfunc <- colorRampPalette(c("grey50", "black"))
str_vec <- arcs2$n %>% max %>% colfunc

# Ajouter les variables pertinentes au network:
net %v% "regime" = comm %$% regime 
net %v% "form" = comm %$% form
net %v% "start" = comm %$% start
net %v% "prog" = comm %$% prog

# Vecteur de poids pour gradient de couleur :
net %e% "colWeight" <- str_vec[arcs2$n] 

# vecteur de poids pour l'épaisseur des arcs:
net %e% "sizeWeight" <- (arcs2$n/(max(arcs2$n)))^0.6

# vecteur de transparence pour rendre les noeuds "inconnus" plus transparents:
net %v% "aNA_start" <- comm %>% 
  mutate(n=case_when(start=="Inconnu" ~ 0.5, TRUE ~ 1)) %$% n

net %v% "aNA_form" <- comm %>% 
  mutate(n=case_when(form=="Inconnu" ~ 0.5, TRUE ~ 1)) %$% n

net %v% "aNA_regime" <- comm %>% 
  mutate(n=case_when(regime=="Inconnu" ~ 0.5, TRUE ~ 1)) %$% n

net %v% "aNA_prog" <- comm %>% 
  mutate(n=case_when(prog=="Inconnu" ~ 0.5, TRUE ~ 1)) %$% n


set.seed(2); ggnet2(net, color="start", palette='Set1', 
                    edge.color="colWeight", 
                    edge.size = "sizeWeight",
                    size=5, node.alpha = "aNA_start")

set.seed(2); ggnet2(net, color="prog", palette='Pastel2', 
                    edge.color="colWeight", 
                    edge.size = "sizeWeight",
                    size=5, node.alpha = "aNA_prog")

set.seed(2); ggnet2(net, color="form", palette='Pastel2', 
                    edge.color="colWeight", 
                    edge.size = "sizeWeight",
                    size=5, node.alpha = "aNA_form")

set.seed(2); ggnet2(net, color="regime", palette='Dark2', 
                    edge.color="colWeight", 
                    edge.size = "sizeWeight",
                    size=5, node.alpha = "aNA_regime")


data.frame(start = comm$annee_debut) %>% 
  filter(!is.na(start)) %>% 
  mutate(start=case_when(start=="A2020" ~ "A2020",
                         TRUE ~ "Autre")) %>% 
  group_by(start) %>% count


# testing igraph functionalities
adj <- get.adjacency(g)
layout <- layout_nicely(g)
plot(g, layout=layout)

#VisNetwork; en cliquant on peut voir le Cci
nodes <- data.frame(id=sort(etudiants$ID), 
                    label=sort(etudiants$ID), 
                    color = "#B3E2CD")
edges <- data.frame(from=arcs[,1], 
                    to=arcs[,2], 
                    value = (arcs[,3]/max(arcs[,3])^0.6),
                    color = "#FDCDAC")
visNetwork(nodes = nodes, edges = edges) %>% visIgraphLayout() %>% 
  visOptions(highlightNearest = list(enabled = TRUE, 
                                     degree = 1,
                                     hover = TRUE,
                                     hideColor = "#F2F2F2",
                                     algorithm = "hierarchical",
                                     labelOnly = FALSE),
             nodesIdSelection = TRUE)

