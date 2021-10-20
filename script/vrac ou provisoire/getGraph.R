######################################################
# Analyse des données de vote de l'assemblée française
######################################################

#ATTENTION A LA MEMOIRE UN DES DATA.FRAME INTERMEDIARE EST DE TAILLE MONSTRUEUSE

library(tidyverse) # Pour avoir tout l'univers tidy
library(tidygraph) #Pour les graphes
library(ggraph) #Pour les graphes
library(igraph) #Pour les graphes
library(statnet)
library(qgraph)
library(RColorBrewer) # Pour graphique
library(lubridate) # Pour graphique
library(data.table) # Pour graphique
library(stringr)
library(dplyr)

  #CHARGEMENT DES DONNEES VOTE FINAL
#On lit le fichier obtenue après getData.R de ce projet
setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data_vote/")
list.files()

vote_final <- read.csv("vote_final.csv" )[,-1]
head(vote_final)

#On s'assure du type que l'on veut donner aux variables
vote_final <-vote_final %>% 
  mutate(vote_code = as.factor(vote_code),
         date_vote = as.Date(date_vote),
         depute_code = as.factor(depute_code),
         nom = as.factor(nom),
         prenom = as.factor(prenom),
         experienceDepute = as.numeric(str_extract(experienceDepute,"\\b([0-9]|[1-9][0-9]|100)\\b")),
         job = as.factor(job),
         groupeAbrev = as.factor(groupeAbrev),
         naissance = as.Date(naissance)) %>%
         filter(groupeAbrev == "FI") %>%
         filter(year(date_vote) == 2018)

# L'idée est de créer un réseau de vote commun, 
# ainsi on va effectuer une jointure de la même table sur le code_loi et le vote code
# Data pour merge
vote_merge <- vote_final 

#Creation data.frame vote commun (Edge)
#Operation delicate
#Ici on calcule le nombre de fois où i et j ont voté la me chose
vote_commun <-  merge(vote_final, vote_merge, by=c("uid_vote","vote_code")) %>%
                filter(depute_code.y != depute_code.x) %>%
                group_by(depute_code.y,depute_code.x) %>%
                count(depute_code.y,depute_code.x)

#Ici on calcule le nombre de fois où i et j ont voté pour la même loi mais pas forcement la même chose
nombre_loi_vote_en_commun <- merge(vote_final, vote_merge, by=c("uid_vote")) %>%
                             filter(depute_code.y != depute_code.x) %>%
                             group_by(depute_code.y,depute_code.x) %>%
                             count(depute_code.y,depute_code.x)



vote_commun <-  merge(vote_commun, nombre_loi_vote_en_commun, by=c("depute_code.x","depute_code.y")) %>%
                mutate(n = n.x/n.y) %>%
                select(depute_code.x,depute_code.y,n,n.y) 

setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data_depute/")
depute_plus_file <- list.files()[1]
depute_plus <- read.csv(depute_plus_file)

nombre_loi_vote <- inner_join(vote_final, vote_merge, by=c("uid_vote","vote_code")) %>% 
  filter(depute_code.y == depute_code.x)%>%  
  group_by(depute_code.y,depute_code.x) %>%
  count(depute_code.y,depute_code.x)%>%
  ungroup()%>% 
  select(depute_code.x,n)%>% 
  rename(id=depute_code.x) %>%
  merge( depute_plus, by = "id") %>% 
  mutate(nom_prenom = paste(as.character(nom)," " ,as.character(prenom))) %>% 
  select(id,nom_prenom,groupeAbrev,n)

names(nombre_loi_vote)<-c("depute_code","nom_prenom","groupe_code","nombre_vote")
names(vote_commun) <- c("from","to","indice","vote_commun")

vote_commun  <- merge(vote_commun,nombre_loi_vote,by.x="from",by.y="depute_code") 
vote_commun  <- merge(vote_commun,nombre_loi_vote,by.x="to",by.y="depute_code") 
vote_commun <- vote_commun %>% 
  mutate(indice = (indice + (vote_commun/(nombre_vote.x) + vote_commun/(nombre_vote.y))/2)/2)%>% 
  mutate(to = nom_prenom.y)%>% 
  mutate(from = nom_prenom.x) %>%
  select(from,to,indice)


code_depute <- unique(vote_commun$from)

for (i in 1:length(code_depute)){
  print(code_depute[i])
  provisoir <- vote_commun %>% 
    filter(to == code_depute[i])
  vote_commun <- vote_commun[!(vote_commun$to %in% provisoir$to & 
                                 vote_commun$from %in% provisoir$from &
                                 vote_commun$from %in% code_depute[1:i]) 
                             ,]
}


nombre_loi_vote <- nombre_loi_vote %>%
  mutate(n= round(nombre_vote/sum(nombre_vote)*100,2))%>% 
  select(nom_prenom,n)


rm(list = c("depute_plus","depute_plus_file","code_depute","i","vote_final","vote_merge","nombre_loi_vote_en_commun","provisoir"))

setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data_graph")

