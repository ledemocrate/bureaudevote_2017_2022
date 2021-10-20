######################################################
# Analyse des données de vote de l'assemblée française
######################################################

#MODIFICATION VOTE CROISE POUR OBTENIR INFORMATION UTILE
# ATTENTION : Couteux en calcul

library(tidyverse) 
library(data.table)
library(stringr)
library(igraph)


setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/out/vote_croise")
list.files()

vote_final_read <- fread("vote_croise_0.88.csv")
vote_final_read <- data.frame(vote_final_read)
head(vote_final_read)

note <- vote_final_read$vote_code.x==vote_final_read$vote_code.y & vote_final_read$depute_code.y != vote_final_read$depute_code.x
note2 <-vote_final_read$depute_code.y != vote_final_read$depute_code.x
note3 <- vote_final_read$depute_code.y == vote_final_read$depute_code.x

vote_commun <-  vote_final_read[note,]
nombre_loi_en_commun <- vote_final_read[note2,]
nombre_loi_vote <- vote_final_read[note3,]

vote_commun <-  vote_commun %>%
  group_by(depute_code.y,depute_code.x) %>%
  count(depute_code.y,depute_code.x)

nombre_loi_en_commun <- nombre_loi_en_commun %>%
  group_by(depute_code.y,depute_code.x) %>%
  count(depute_code.y,depute_code.x)

vote_commun <-  merge(vote_commun, nombre_loi_en_commun, by=c("depute_code.x","depute_code.y")) %>%
  mutate(n = n.x/n.y) %>%
  select(depute_code.x,depute_code.y,n,n.y,n.x) 

names(vote_commun) <- c("from","to","indice","loi_commune","vote_commun")


nombre_loi_vote <- nombre_loi_vote %>% 
  group_by(depute_code.x,nom_prenom.x,groupeAbrev.x,job.x,experienceDepute.x,naissance.x) %>%
  count(depute_code.x,nom_prenom.x,groupeAbrev.x)%>%
  ungroup()%>% 
  rename(depute_code=depute_code.x,
         nom_prenom=nom_prenom.x,
         groupe_code = groupeAbrev.x,
         job = job.x,
         experienceDepute = experienceDepute.x,
         naissance = naissance.x,
         nombre_vote=n)

nombre_depute_groupe <- nrow(nombre_loi_vote)


vote_commun  <- merge(vote_commun,nombre_loi_vote,by.x="from",by.y="depute_code")  
vote_commun  <- merge(vote_commun,nombre_loi_vote,by.x="to",by.y="depute_code") 

vote_commun  <- vote_commun %>% 
  mutate(indice_taux_connectivité =  loi_commune/(nombre_vote.x+nombre_vote.y- loi_commune)) %>% 
  mutate(indice_intensité_connectivité = indice) %>% 
  mutate(to = nom_prenom.y)%>% 
  mutate(from = nom_prenom.x) %>% 
  select(from,to,loi_commune,vote_commun,nombre_vote.x,nombre_vote.y,indice_taux_connectivité,indice_intensité_connectivité)

nombre_loi_vote <- nombre_loi_vote %>%
  filter(nom_prenom %in% unique(as.character(vote_commun$from)) | nom_prenom %in% unique(as.character(vote_commun$to)) )

rm(list=c("note","note2","note3","vote_final_read","nombre_loi_en_commun"))


g <- as_data_frame(simplify(graph_from_data_frame(vote_commun, 
                           directed=FALSE)))
vote_commun <- inner_join(g,vote_commun,by=c("to","from")) %>%
  mutate(indice_connectivité = indice_intensité_connectivité*indice_taux_connectivité)

ed <-  data.frame(nom_prenom=unique(as.character(unlist(vote_commun[, c("from", "to")])))) ;
nombre_loi_vote <- merge(ed, nombre_loi_vote, by="nom_prenom", all=TRUE) %>%
  select(-depute_code)


numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

choix_echantillon_base_nombre_votant <- numextract("vote_croise_0.88.csv")

setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/out/graph")

fwrite(vote_commun,paste0("edge_",choix_echantillon_base_nombre_votant,".csv"))
fwrite(nombre_loi_vote,paste0("node_",choix_echantillon_base_nombre_votant,".csv"))

