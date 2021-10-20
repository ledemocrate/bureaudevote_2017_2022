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

k_seq <- c(seq(0,1,0.01))

groupe_seq <- unique(as.character(node$groupe_code))
groupe_seq_1 <- expand.grid(groupe_seq,groupe_seq,k_seq)%>%
  filter(Var1!=Var2)%>%
  rename(from=Var1,to=Var2,k=Var3)%>%
  arrange(from,to,k)

groupe_seq_bis <-as_data_frame(simplify(graph_from_data_frame(groupe_seq_1, directed=FALSE)))%>% 
  inner_join(groupe_seq_1,by=c("from","to"))

  
fonction_indice_proximite_inter_groupe <- function(k,groupe1,groupe2) {
  
  node_select <-node %>% 
    filter(groupe_code == groupe1 |groupe_code == groupe2)
  
  nombre_depute_groupe <- nrow(node_select)
  
  edge_select <- merge(edge,node_select,by.x="from",by.y="nom_prenom") 
  edge_select <-merge(edge_select,node_select,by.x="to",by.y="nom_prenom") %>%
    filter(groupe_code.x != groupe_code.y) %>%
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
    indice_final <- data.frame(groupe1=groupe1,
                               groupe2=groupe2,
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
    
    node_groupe_1 <- node_select %>%
      group_by(groupe_code)%>%
      count()
    groupe_max <- min(node_groupe_1$n)
    edge_max <- node_groupe_1$n[1]*node_groupe_1$n[2]
    
    indice_final <- data.frame(groupe1=groupe1,
                               groupe2=groupe2,
                               k=k,
                               noeud_max = nombre_depute_groupe,
                               nombre_noeud = length(V(g)),
                               edge_max = edge_max,
                               nombre_edge = length(E(g)),
                               groupe_max = groupe_max,
                               nombre_groupe = length(commu)) %>%
      mutate(indice_noeud = length(V(g))/nombre_depute_groupe,
             indice_lien = length(E(g))/edge_max,
             indice_groupe = case_when(length(commu)==1 & groupe_max>1 ~ 1,
                                       length(commu)==1 & groupe_max==1 ~ 0,
                                       length(commu)>1 ~ 1-length(commu)/floor(length(V(g))/2)),
             indice_totale = indice_noeud*indice_lien*indice_groupe,
             indice_moyen=(indice_noeud+indice_lien+indice_groupe)/3)
    
    
    return(list(indice_final))
  }
}


indice <- bind_rows(mapply(fonction_indice_proximite_inter_groupe,groupe_seq_bis[[3]],groupe_seq_bis[[2]],groupe_seq_bis[[1]]))


setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/out/indice/indice_cohesion_intra_groupe")
fwrite(indice,"indice_inter.csv")
