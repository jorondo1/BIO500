library(pacman)
p_load(tidygraph, igraph, dplyr, GGally, network, sna, intergraph, magrittr,
       RColorBrewer, NetworkToolbox, sparsebnUtils, spaa, tibble, rstatix, ggpubr)
set.seed(04042023)
# 
# # Create edge list
# w <- collaborations %>% count(etudiant1,etudiant2) %>% 
#   filter(etudiant1!=etudiant2) %>% # remove self-edges
#   # remove duplicate from list
#   mutate(id = case_when(etudiant1<etudiant2 ~ paste0(etudiant1,"+",etudiant2),
#                         etudiant2<etudiant1 ~ paste0(etudiant2,"+",etudiant1))) %>% 
#   distinct(id, .keep_all=TRUE) %>% mutate_if(is.character, as.factor)
# 
# am <- list2dist(w) %>% as.matrix  %>% replace(is.na(.),0)

# Edge list without removing duplicates:
w <- collaborations %>% count(etudiant1,etudiant2) %>% 
  filter(etudiant1!=etudiant2) %>% mutate_if(is.character, as.factor) %>% as.matrix

am <- list2dist(w) %>% as.matrix %>% replace(is.na(.),0)
am %>% isSymmetric

ggplot(as.data.frame(w2), aes(x=etudiant1,y=etudiant2,fill=n)) + geom_tile() +
  ylim(rev(levels(w$etudiant2))) + xlim(levels(w$etudiant1))
# 
# el <- as.matrix(w[,1:2])
# g <- make_graph(edges=el, directed = FALSE)
# #g <- set.edge.attribute(g, "weight", value = w$n)
# 
# hist(degree.distribution(g))
# layout <- layout_with_kk(g)
# plot(g, layout=layout)
# summary(g)
# 

### COMPUTE CCi pour tous les noeuds
cci <- clustcoeff(am, weighted=TRUE) %$% CCi %>% 
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


(cciReg.plot <- cciGroup %>% filter(!is.na(coop)) %>% 
  ggplot(aes(x = coop, y = cci, color = coop)) +
  geom_boxplot() + geom_point(position="jitter") +
  theme_light() +
  theme(legend.position="none")+ # Hide legend
  stat_pvalue_manual(reg.stat,  
                     label = "p.adj.signif",
                     y.position = 2.2) + # Add stat test
  labs(x = "Régime Coop", y = "Coefficient de regroupement",
       title = "Distribution des coefficients de regroupement individuels par type de programme.")
)


(cciYr.plot <- cciGroup %>% filter(!is.na(yr)) %>% 
  ggplot(aes(x = yr, y = cci, color=yr)) +
  geom_boxplot() + geom_point(position="jitter") +
  theme_light() +
    theme(legend.position="none")+ # Hide legend
    stat_pvalue_manual(yr.stat,  
                       label = "p.adj.signif",
                       y.position = 2.2) + # Add stat test
    labs(x = "Première session", y = "Coefficient de regroupement",
         title = "Distribution des coefficients de regroupement individuels par session de début de bac.")
)


### NETWORK PLOTS

## using ggnet, plot by regime
net = network(as.matrix(w[,1:2]), directed = FALSE) 
net %v% "regime" = arrange(etudiants, by = ID) %$% regime_coop
ggnet2(net, size=6, weight = w$n, color="regime", palette = "Set2")

## plot by formation prealable 
net %v% "form" = arrange(etudiants, by = ID) %$% formation_prealable
ggnet2(net, size=6, weight = w$n, color="form", palette = "Set2")

net %v% "start" = arrange(etudiants, by = ID) %$% annee_debut
net %v% "prog" = arrange(etudiants, by = ID) %$% programme

## with labels
ggnet2(net, label = etudiants$ID %>% sort, label.size = 3,
       color = "form", palette = "Set3", segment.alpha=0.2,
       segment.color = "grey90", size="degree", size.cut=TRUE,
       segment.size = 0.1)


data.frame(start = etudiants$annee_debut) %>% 
  filter(!is.na(start)) %>% 
  mutate(start=case_when(start=="A2020" ~ "A2020",
                         TRUE ~ "Autre")) %>% 
  group_by(start) %>% count

