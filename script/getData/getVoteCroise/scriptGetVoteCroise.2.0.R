######################################################
# Analyse des données de vote de l'assemblée française
######################################################

library(tidyverse) 
library(data.table)
library(igraph)

setwd("C:/Users/GoldentzGrahamz/OneDrive/Documents/GitHub/bureaudevote/data/data_final/")
list.files()

vote_final <- read.csv("vote_final.csv" )[,-1]

loi_seq <- as.character(unique(vote_final$uid_loi)) 

fonction_vote_croise <- function(loi_uid){
  
  vote_final_select <- vote_final %>% 
    filter(uid_loi == loi_uid)%>% 
    mutate(vote_code = as.factor(vote_code),
           nom = as.factor(nom),
           prenom = as.factor(prenom),
           experienceDepute = as.numeric(str_extract(experienceDepute,"\\b([0-9]|[1-9][0-9]|100)\\b")),
           job = as.factor(job),
           groupeAbrev = as.factor(groupeAbrev),
           naissance = as.Date(naissance)) 
  
  vote_final_select <- inner_join(vote_final_select, vote_final_select, by=c("uid_loi"))%>%
    mutate(vote_commun = case_when(vote_code.x==vote_code.y~1,
                                   vote_code.x!=vote_code.y~0),
           loi_commun = 1) %>%
    rename(from =nom_prenom.x,to=nom_prenom.y )%>%
    select(from,to,vote_commun,loi_commun)
  
  g <- as_data_frame(simplify(graph_from_data_frame(vote_final_select, 
                                                    directed=FALSE)))
  vote_final_select <- inner_join(g,vote_final_select,by=c("to","from"))
  
  
  assign(paste0("vote_croise_","",loi_uid,sep=""),
         vote_final_select)
  
  
  fwrite(get(paste0("vote_croise_","",loi_uid,sep="")),
         paste0("vote_croise_","",loi_uid,".csv"))
  
  rm(list=c(paste0("vote_croise_","",loi_uid,sep=""),"vote_final_select"))
  
}

setwd("C:/Users/GoldentzGrahamz/OneDrive/Documents/GitHub/bureaudevote/data/data_vote_croise/")
lapply(loi_seq,fonction_vote_croise)

