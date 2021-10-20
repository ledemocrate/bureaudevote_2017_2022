######################################################
# Analyse des données de vote de l'assemblée française
######################################################

library(tidyverse) # Pour avoir tout l'univers tidy
library(igraph) #Pour les graphes
library(data.table) #Pour les graphes

setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/out/graph/")
list.files()

edge <- read.csv("edge.csv") %>%
  select(from,to,indice_connectivité_transformé) %>%
  rename(indice=indice_connectivité_transformé)

node <- read.csv("node.csv") 

# On va créer une fonction mais pour cela on spécifie d'abord le domaine de définition de cette fonction
# Le cutt-off pour la prise en compte de lien
k_seq <- c(seq(0,1,0.01))
#Les groupes politique
groupe_seq <- unique(as.character(node$groupe_code))

fonction_indice_proximite_intra_groupe <- function(k,groupe) {
  
  node_select <-node %>% 
    filter(groupe_code == groupe)
  
  nombre_depute_groupe <- nrow(node_select)
  
  edge_select <- edge %>%
    filter(from %in% node_select$nom_prenom & to %in% node_select$nom_prenom )%>%
    filter(indice>k)
  
  inidce_centralite_part_1 <- edge_select %>%
    group_by(from)%>%
    summarize(inidce_centralite=sum(indice),.groups = 'drop')
  
  inidce_centralite_part_2 <- edge_select %>%
    group_by(to)%>%
    summarize(inidce_centralite=sum(indice),.groups = 'drop')
  
  indice_centralite <- merge(inidce_centralite_part_1,inidce_centralite_part_2,by.x="from",by.y="to",all=TRUE)%>%
    mutate(inidce_centralite.x=case_when(is.na(inidce_centralite.x)~ 0,
                                         !is.na(inidce_centralite.x) ~ inidce_centralite.x))%>%
    mutate(inidce_centralite.y=case_when(is.na(inidce_centralite.y)~ 0,
                                         !is.na(inidce_centralite.y) ~ inidce_centralite.y)) %>%
    mutate(indice_centralite=inidce_centralite.x+inidce_centralite.y) %>%
    rename(nom_prenom=from)%>%
    select(nom_prenom,indice_centralite)
  
  node_select <- node_select %>% inner_join(indice_centralite,by="nom_prenom") 
  
  
  
  node_select <- node_select %>%
    filter(nom_prenom %in% edge_select$from | nom_prenom %in% edge_select$to )
  
  if (nrow(edge_select)==0) {
    indice_final <- data.frame(groupe=groupe,
                               k=k,
                               noeud_max = nombre_depute_groupe,
                               nombre_noeud=0,
                               edge_max=0,
                               nombre_edge=0,
                               groupe_max = 0,
                               nombre_groupe=0,
                               indice_noeud = 0 ,
                               indice_lien = 0,
                               indice_groupe =0,
                               indice_totale=0,
                               indice_moyen=0)
    return(list(indice_final))
  }
  
  if (nrow(edge_select)>0) {
    g <- graph_from_data_frame(edge_select,
                               vertices=node_select, 
                               directed=FALSE) %>% set_vertex_attr("nom_prenom", value = node_select$nom_prenom)
    
    isolated <- which(igraph::degree(g)==0)
    g <-  igraph::delete.vertices(g, isolated)
    
    E(g)$weights <-edge_select$indice
    V(g)$weights <- node_select$indice_centralite
    
    lc <- igraph::cluster_infomap(g, e.weights= E(g)$weights,v.weights=V(g)$weights ) 
    commu<- communities(lc)
    
    indice_final <- data.frame(groupe=groupe,
                               k=k,
                               noeud_max = nombre_depute_groupe,
                               nombre_noeud = length(V(g)),
                               edge_max = ((length(V(g))*(length(V(g))-1))/2),
                               nombre_edge = length(E(g)),
                               groupe_max = floor(length(V(g))/2),
                               nombre_groupe = length(commu)) %>%
      mutate(indice_noeud = length(V(g))/nombre_depute_groupe,
             indice_lien = length(E(g))/((length(V(g))*(length(V(g))-1))/2),
             indice_groupe = case_when(length(commu)==1 & groupe_max>1 ~ 1,
                                       length(commu)==1 & groupe_max==1 ~ 0,
                                       length(commu)>1 ~ 1-length(commu)/floor(length(V(g))/2)),
             indice_totale = indice_noeud*indice_lien*indice_groupe,
             indice_moyen=(indice_noeud+indice_lien+indice_groupe)/3)
    
    
    return(list(indice_final))
  }
}
fonction_indice_proximite_globale <- function(k) {
  
  node_select <-node 
  
  nombre_depute_groupe <- nrow(node_select)
  
  edge_select <- edge %>%
    filter(from %in% node_select$nom_prenom & to %in% node_select$nom_prenom )%>%
    filter(indice>k)
  
  inidce_centralite_part_1 <- edge_select %>%
    group_by(from)%>%
    summarize(inidce_centralite=sum(indice),.groups = 'drop')
  
  inidce_centralite_part_2 <- edge_select %>%
    group_by(to)%>%
    summarize(inidce_centralite=sum(indice),.groups = 'drop')
  
  indice_centralite <- merge(inidce_centralite_part_1,inidce_centralite_part_2,by.x="from",by.y="to",all=TRUE)%>%
    mutate(inidce_centralite.x=case_when(is.na(inidce_centralite.x)~ 0,
                                         !is.na(inidce_centralite.x) ~ inidce_centralite.x))%>%
    mutate(inidce_centralite.y=case_when(is.na(inidce_centralite.y)~ 0,
                                         !is.na(inidce_centralite.y) ~ inidce_centralite.y)) %>%
    mutate(indice_centralite=inidce_centralite.x+inidce_centralite.y) %>%
    rename(nom_prenom=from)%>%
    select(nom_prenom,indice_centralite)
  
  node_select <- node_select %>% inner_join(indice_centralite,by="nom_prenom") 
  
  
  
  node_select <- node_select %>%
    filter(nom_prenom %in% edge_select$from | nom_prenom %in% edge_select$to )
  
  if (nrow(edge_select)==0) {
    indice_final <- data.frame(groupe="global",
                               k=k,
                               noeud_max = nombre_depute_groupe,
                               nombre_noeud=0,
                               edge_max=0,
                               nombre_edge=0,
                               groupe_max = 0,
                               nombre_groupe=0,
                               indice_noeud = 0 ,
                               indice_lien = 0,
                               indice_groupe =0,
                               indice_totale=0,
                               indice_moyen=0)
    return(list(indice_final))
  }
  
  if (nrow(edge_select)>0) {
    g <- graph_from_data_frame(edge_select,
                               vertices=node_select, 
                               directed=FALSE) %>% set_vertex_attr("nom_prenom", value = node_select$nom_prenom)
    
    isolated <- which(igraph::degree(g)==0)
    g <-  igraph::delete.vertices(g, isolated)
    
    E(g)$weights <-edge_select$indice
    V(g)$weights <- node_select$indice_centralite
    
    lc <- igraph::cluster_infomap(g, e.weights= E(g)$weights,v.weights=V(g)$weights ) 
    commu<- communities(lc)
    
    indice_final <- data.frame(groupe="global",
                               k=k,
                               noeud_max = nombre_depute_groupe,
                               nombre_noeud = length(V(g)),
                               edge_max = ((length(V(g))*(length(V(g))-1))/2),
                               nombre_edge = length(E(g)),
                               groupe_max = floor(length(V(g))/2),
                               nombre_groupe = length(commu)) %>%
      mutate(indice_noeud = length(V(g))/nombre_depute_groupe,
             indice_lien = length(E(g))/((length(V(g))*(length(V(g))-1))/2),
             indice_groupe = case_when(length(commu)==1 & groupe_max>1 ~ 1,
                                       length(commu)==1 & groupe_max==1 ~ 0,
                                       length(commu)>1 ~ 1-length(commu)/floor(length(V(g))/2)),
             indice_totale = indice_noeud*indice_lien*indice_groupe,
             indice_moyen=(indice_noeud+indice_lien+indice_groupe)/3)
    
    
    return(list(indice_final))
  }
}

indice <- lapply(groupe_seq,function(groupe_bis){lapply(k_seq,fonction_indice_proximite_intra_groupe,groupe_bis)})
indice <- lapply(1:13,function(i){bind_rows(indice[[i]])})
indice <- bind_rows(indice)

indice_global <- lapply(k_seq,fonction_indice_proximite_globale)
indice_global <- bind_rows(indice_global)
indice <- rbind(indice,indice_global)

setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/out/indice/indice_cohesion_intra_groupe")
fwrite(indice,"indice_intra.csv")
