
library(tidyverse) 
library(igraph) 
library(statnet)
library(ggplot2)
library(DescTools)

setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/out/indice/indice_cohesion_intra_groupe")
list.files()

indice_intra <- read.csv("indice_intra.csv")

choix_groupe <- "DEM"
indice_proximite_groupe <- indice_intra %>%
  filter(groupe == choix_groupe)

###Indice totale
graphique <- indice_proximite_groupe %>% 
  ggplot(aes(x = k, y = indice_totale )) +
  geom_line()  +
  geom_area()

AUC <- paste("AUC : ", round(AUC(indice_proximite_groupe$k,indice_proximite_groupe$indice_totale),2))

graphique + 
  geom_text(mapping = aes(x = 0.6, y = 0.5, label = AUC)) +
  ggtitle(paste("Indice de cohésion du groupe ",choix_groupe)) +
  theme(plot.title = element_text(hjust = 0.5))


##Groupe
graphique <- indice_proximite_groupe %>% 
  ggplot(aes(x = k, y = indice_groupe )) +
  geom_line()  +
  geom_area()

AUC <- paste("AUC : ", round(AUC(indice_proximite_groupe$k,indice_proximite_groupe$indice_groupe),2))

graphique + 
  geom_text(mapping = aes(x = 0.6, y = 0.5, label = AUC)) +
  ggtitle(paste("Indice de clustering du groupe ",choix_groupe)) +
  theme(plot.title = element_text(hjust = 0.5))

####Noeud
graphique <- indice_proximite_groupe %>% 
  ggplot(aes(x = k, y = indice_noeud )) +
  geom_line()  +
  geom_area()

AUC <- paste("AUC : ", round(AUC(indice_proximite_groupe$k,indice_proximite_groupe$indice_noeud),2))

graphique + 
  geom_text(mapping = aes(x = 0.6, y = 0.5, label = AUC)) +
  ggtitle(paste("Indice du nombre de noeud du groupe ",choix_groupe)) +
  theme(plot.title = element_text(hjust = 0.5))

####Lien
graphique <- indice_proximite_groupe %>% 
  ggplot(aes(x = k, y = indice_lien )) +
  geom_line()  +
  geom_area()

AUC <- paste("AUC : ", round(AUC(indice_proximite_groupe$k,indice_proximite_groupe$indice_lien),2))

graphique + 
  geom_text(mapping = aes(x = 0.6, y = 0.5, label = AUC)) +
  ggtitle(paste("Indice du nombre de lien du groupe ",choix_groupe)) +
  theme(plot.title = element_text(hjust = 0.5))

####Indice moyen
graphique <- indice_proximite_groupe %>% 
  ggplot(aes(x = k, y = indice_moyen )) +
  geom_line()  +
  geom_area()

AUC <- paste("AUC : ", round(AUC(indice_proximite_groupe$k,indice_proximite_groupe$indice_moyen),2))

graphique + 
  geom_text(mapping = aes(x = 0.6, y = 0.5, label = AUC)) +
  ggtitle(paste("Moyenne des indice",choix_groupe)) +
  theme(plot.title = element_text(hjust = 0.5))
