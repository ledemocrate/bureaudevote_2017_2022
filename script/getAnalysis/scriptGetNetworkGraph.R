

library(tidyverse) # Pour avoir tout l'univers tidy
library(igraph) #Pour les graphes


setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/out/graph/")
list.files()

edge <- read.csv("edge_0.88.csv") %>%
  select(from,to,indice_connectivité) %>%
  rename(indice=indice_connectivité)

node <- read.csv("node_0.88.csv") 

# On va créer une fonction mais pour cela on spécifie d'abord le domaine de définition de cette fonction
# Le cutt-off pour la prise en compte de lien
k_seq <- c(seq(0,1,0.01))
#Les groupes politique
groupe_seq <- unique(as.character(node$groupe_code))

fonction_creation_graph <- function(k,groupe) {
  
  node_select <-node %>% 
    filter(groupe_code == groupe)
  
  nombre_depute_groupe <- nrow(node_select)
  
if(nrow(node_select)>20) {

  node_select <- node_select[sample(nrow(node_select), 20,replace = FALSE), ]
  edge_select <- edge %>%
      filter(from %in% node_select$nom_prenom & to %in% node_select$nom_prenom) %>%
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
}
else{
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
}
 
if (nrow(edge_select)==0) {
    return(NA)
}
  
if (nrow(edge_select)>0) {
    
    g <- graph_from_data_frame(edge_select,
                               vertices=node_select, 
                               directed=FALSE) %>% set_vertex_attr("nom_prenom", value = node_select$nom_prenom)
    
    isolated <- which(igraph::degree(g)==0)
    g <-  igraph::delete.vertices(g, isolated)
    
    E(g)$weights <-edge_select$indice
    V(g)$weights <- node_select$indice_centralite
    
    lc <- igraph::cluster_infomap(g, e.weights= edge_select$indice,v.weights=V(g)$weights ) 
    commu<- communities(lc)
    V(g)$color=membership(lc)
    c_scale <- colorRamp(c('red','yellow','cyan','blue'))
    E(g)$color = apply(c_scale(E(g)$weights), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) )
    
    E(g)$weights <-edge_select$indice +(edge_select$indice-mean(edge_select$indice))
    
    plot(g,
         vertex.size=V(g)$weights,
         vertex.label.cex=.6,
         vertex.label.color="black",
         vertex.shape="circle",# "circle"vertex.label.color="black",# Paramètres des arêtesedge.arrow.size=0.5,edge.width=E(x)$weight,edge.color=rgb(.1,.1,.1,.8),# Paramètres générauxlayout= x$layout,main="Titre du graphe",sub="sous titre")
         edge.width=E(g)$weights,
         main=paste0("Graphe inter-vote des députés intra-",groupe, " pour la valuer de lien minimum de ",as.character(k)))
  }
}

fonction_creation_graph(0.9,"LR")
indice <- lapply(groupe_seq,function(groupe_bis){lapply(k_seq,fonction_indice_proximite_groupe,groupe_bis)})
