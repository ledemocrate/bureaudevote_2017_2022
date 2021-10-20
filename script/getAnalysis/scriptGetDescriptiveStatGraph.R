######################################################
# Analyse des données de vote de l'assemblée française
######################################################

library(tidyverse) 
library(data.table)

setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/out/graph")
list.files()

vote_commun <- fread("edge.csv")
nombre_loi_vote <- fread("node.csv")

#Petit apreçu des données
head(vote_commun)

#Creation d'information utile
seq_theorique <- seq(0,1,0.01) 
K <- 8
K2 <- 2
transformation_theorique_data_plot <- data.frame(transformation = 1/(1+exp(-8*(seq_theorique-mean(vote_commun$indice_taux_connectivité))))**K2,
                                                 k = seq_theorique )

transformation_theorique_data_plot %>% ggplot(aes(x=k,y =transformation,k=k))+
  geom_line(aes(colour="red"))+
  geom_line(aes(x=k,y=k))+
  xlab("Valeur du taux")+
  ylab("Valeur du taux transformé")+
  ggtitle("Transformation logistique d'un taux")+ 
  labs(caption = paste0("K = ",
                        as.character(K),
                        " & x0 tel que mean(vote_commun$indice_taux_connectivité) soit ",
                        as.character(mean(vote_commun$indice_taux_connectivité))))



vote_commun_intra_groupe <- vote_commun %>%
  merge(nombre_loi_vote, by.x = "from", by.y="nom_prenom") %>%
  select(colnames(vote_commun),groupe_code)%>%
  rename(groupe_code.x=groupe_code)%>%
  merge(nombre_loi_vote, by.x = "to", by.y="nom_prenom")%>%
  select(colnames(vote_commun),groupe_code.x,groupe_code)%>%
  rename(groupe_code.y=groupe_code)%>%
  filter(groupe_code.y==groupe_code.x)

vote_commun_inter_groupe <- vote_commun %>%
  merge(nombre_loi_vote, by.x = "from", by.y="nom_prenom") %>%
  select(colnames(vote_commun),groupe_code)%>%
  rename(groupe_code.x=groupe_code)%>%
  merge(nombre_loi_vote, by.x = "to", by.y="nom_prenom")%>%
  select(colnames(vote_commun),groupe_code.x,groupe_code)%>%
  rename(groupe_code.y=groupe_code)%>%
  filter(groupe_code.y!=groupe_code.x)

#########################################
#Taux de connectivité
#########################################

hist(vote_commun$indice_taux_connectivité, 
     main = "Distribution du taux de connectivité", 
     ylab = "Nombre de relations",
     xlab= "Taux de connectivité",
     col = "ivory")
mean(vote_commun$indice_taux_connectivité)
var(vote_commun$indice_taux_connectivité)

hist(vote_commun_intra_groupe$indice_taux_connectivité, 
     main = "Distribution du taux de connectivité intra-groupe", 
     ylab = "Nombre de relations",
     xlab= "Taux de connectivité",
     col = "ivory")
mean(vote_commun_intra_groupe$indice_taux_connectivité)
var(vote_commun_intra_groupe$indice_taux_connectivité)

hist(vote_commun_inter_groupe$indice_taux_connectivité, 
     main = "Distribution du taux de connectivité inter-groupe", 
     ylab = "Nombre de relations",
     xlab= "Taux de connectivité",
     col = "ivory")
mean(vote_commun_inter_groupe$indice_taux_connectivité)
var(vote_commun_inter_groupe$indice_taux_connectivité)

hist(1/(1+exp(-K*(vote_commun$indice_taux_connectivité-mean(vote_commun$indice_taux_connectivité)))**K2), 
                            main = "Distribution du taux de connectivité transformé", 
                            ylab = "Nombre de relations",
                            xlab= "Taux de connectivité transformé",
                            col = "ivory")

hist(1/(1+exp(-K*(vote_commun_intra_groupe$indice_taux_connectivité-mean(vote_commun$indice_taux_connectivité))))**K2, 
                           main = "Distribution du taux de connectivité intra-groupe transformé", 
                           ylab = "Nombre de relations",
                           xlab= "Taux de connectivité transformé",
                           col = "ivory")

hist(1/(1+exp(-K*(vote_commun_inter_groupe$indice_taux_connectivité-mean(vote_commun$indice_taux_connectivité))))**K2, 
     main = "Distribution du taux de connectivité inter-groupe transformé", 
     ylab = "Nombre de relations",
     xlab= "Taux de connectivité transformé",
     col = "ivory")


###################################################
#On s'intéresse à l'intensité  de la connectivité
###################################################

hist(vote_commun$indice_intensité_connectivité, 
     main = "Distribution de l'intensité de la connectivité", 
     ylab = "Nombre de relations",
     xlab=  "Intensité de la connectivité",
     col =  "ivory")
mean(vote_commun$indice_intensité_connectivité)
var(vote_commun$indice_intensité_connectivité)

hist(vote_commun_intra_groupe$indice_intensité_connectivité, 
     main = "Distribution de l'intensité de la connectivité intra-groupe", 
     ylab = "Nombre de relations",
     xlab= "Taux de connectivité",
     col = "ivory")
mean(vote_commun_intra_groupe$indice_intensité_connectivité)
var(vote_commun_intra_groupe$indice_intensité_connectivité)

hist(vote_commun_inter_groupe$indice_intensité_connectivité, 
     main = "Distribution de l'intensité de la connectivité inter-groupe", 
     ylab = "Nombre de relations",
     xlab= "Taux de connectivité",
     col = "ivory")
mean(vote_commun_inter_groupe$indice_intensité_connectivité)
var(vote_commun_inter_groupe$indice_intensité_connectivité)

data_plot_indice_croise <- vote_commun %>%
  mutate(indice_taux_connectivité =round(indice_taux_connectivité,1),indice_intensité_connectivité=round(indice_intensité_connectivité,1) )%>%
  group_by(indice_taux_connectivité,indice_intensité_connectivité)%>%
  count() 

