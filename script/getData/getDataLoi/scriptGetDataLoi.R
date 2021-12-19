######################################
# Création des fichiers amendement/loi
######################################

#LIBRAIRIE UTILISEE

library(rvest) #Pour le web scrapping
library(stringr) #Pour la manipulation textuelle
library(tidyverse) #Couteau suisse
library(purrr) #Pour certaine fonction
library(data.table)

# Récupération du lien amendement/loi sur le site :
# https://www2.assemblee-nationale.fr/scrutins/liste/(offset)/{sequence_offset}/(legislature)/15/(type)/SOR/(idDossier)/TOUS

# Vérifié manuellement que la sequence_offset prend bien en compte l'ensemble des pages
sequence_offset <- c(1,c(1:42)*100)

#Fonction permettant de récupérer les données d'une page
function_get_url_dosier_scrutin <- function(url){
  print(url)
  url_prov <- paste0("https://www2.assemblee-nationale.fr/scrutins/liste/(offset)/",
                     url,
                     "/(legislature)/15/(type)/SOR/(idDossier)/TOUS")
  scrutins <- read_html(url_prov)
  
  prov<- scrutins %>% html_nodes("td")%>% 
    html_text2()
  prov <- prov[-c(1:3)]
  
  url_dossier_associe_prov<- scrutins %>% html_nodes("a")%>% 
    html_attr('href') 
  url_dossier_associe <- c()
  
  url_dossier_associe1 <- url_dossier_associe_prov[str_detect(url_dossier_associe_prov, "^https://www.assemblee-nationale.fr/dyn/15/dossiers/")]
  if(!is_empty(url_dossier_associe1)){
    url_dossier_associe1 <- paste0(url_dossier_associe1,"?etape=15-AN1-DEPOT",sep="")
    url_dossier_associe <- c(url_dossier_associe,url_dossier_associe1)
  }
  
  url_dossier_associe2 <- url_dossier_associe_prov[str_detect(url_dossier_associe_prov, "^https://www.assemblee-nationale.fr/dyn/14/dossiers/")]
  if(!is_empty(url_dossier_associe2)){
    url_dossier_associe1 <- paste0(url_dossier_associe2,"?etape=15-AN1-DEPOT",sep="")
    url_dossier_associe <- c(url_dossier_associe,url_dossier_associe2)
  }
  
  url_dossier_associe3 <- url_dossier_associe_prov[str_detect(url_dossier_associe_prov, "^https://www.assemblee-nationale.fr/15/dossiers/")]
  if(!is_empty(url_dossier_associe3)){
    url_dossier_associe1 <- paste0(url_dossier_associe3,"?etape=15-AN1-DEPOT",sep="")
    url_dossier_associe <- c(url_dossier_associe,url_dossier_associe3)
  }
  
  
  variable1 <- rep(1,100)
  variable2 <- rep(2,100)
  variable3 <- rep(3,100)
  variable4 <- rep(4,100)
  variable5 <- rep(5,100)
  variable6 <- rep(6,100)
  
  
  for(i in 2:100){
    variable1[i] <- variable1[i-1]+6
    variable2[i] <- variable2[i-1]+6
    variable3[i] <- variable3[i-1]+6
    variable4[i] <- variable4[i-1]+6
    variable5[i] <- variable5[i-1]+6
    variable6[i] <- variable6[i-1]+6
    
  }
  
  
  scrutin_numero <- prov[variable1]
  date_scrutin <- prov[variable2]
  resume_legislatif_associe <- prov[variable3]
  
  
  scrutin_dossier_data_prov <- data.frame(scrutin_numero,date_scrutin,resume_legislatif_associe)
  scrutin_dossier_data_prov <- scrutin_dossier_data_prov[str_detect(scrutin_dossier_data_prov$resume_legislatif_associe,"dossier"),]
  
  scrutin_dossier_data <- data.frame(scrutin_dossier_data_prov,url_dossier_associe)
  return(scrutin_dossier_data)
}
#Application de la fonction pour chaque page
dossier_scrutin <- lapply(sequence_offset,function_get_url_dosier_scrutin)
#Création d'un data.frame
dossier_scrutin <- bind_rows(dossier_scrutin)
#On s'assure du type de certaine variable
dossier_scrutin$url_dossier_associe <- as.character(dossier_scrutin$url_dossier_associe)
head(dossier_scrutin)

##########################Exception a la page 3800################################
url_prov <- paste0("https://www2.assemblee-nationale.fr/scrutins/liste/(offset)/",
                   "3800",
                   "/(legislature)/15/(type)/SOR/(idDossier)/TOUS")
scrutins <- read_html(url_prov)

prov<- scrutins %>% html_nodes("td")%>% 
  html_text2()
prov <- prov[-c(1:3)]

url_dossier_associe_prov<- scrutins %>% html_nodes("a")%>% 
  html_attr('href') 
url_dossier_associe <- c()

url_dossier_associe1 <- url_dossier_associe_prov[str_detect(url_dossier_associe_prov, "^https://www.assemblee-nationale.fr/dyn/15/dossiers/")]
if(!is_empty(url_dossier_associe1)){
  url_dossier_associe1 <- paste0(url_dossier_associe1,"?etape=15-AN1-DEPOT",sep="")
  url_dossier_associe <- c(url_dossier_associe,url_dossier_associe1)
}

url_dossier_associe2 <- url_dossier_associe_prov[str_detect(url_dossier_associe_prov, "^https://www.assemblee-nationale.fr/dyn/14/dossiers/")]
if(!is_empty(url_dossier_associe2)){
  url_dossier_associe1 <- paste0(url_dossier_associe2,"?etape=15-AN1-DEPOT",sep="")
  url_dossier_associe <- c(url_dossier_associe,url_dossier_associe2)
}

url_dossier_associe3 <- url_dossier_associe_prov[str_detect(url_dossier_associe_prov, "^https://www.assemblee-nationale.fr/15/dossiers/")]
if(!is_empty(url_dossier_associe3)){
  url_dossier_associe1 <- paste0(url_dossier_associe3,"?etape=15-AN1-DEPOT",sep="")
  url_dossier_associe <- c(url_dossier_associe,url_dossier_associe3)
}


variable1 <- rep(1,100)
variable2 <- rep(2,100)
variable3 <- rep(3,100)
variable4 <- rep(4,100)
variable5 <- rep(5,100)
variable6 <- rep(6,100)


for(i in 2:100){
  variable1[i] <- variable1[i-1]+6
  variable2[i] <- variable2[i-1]+6
  variable3[i] <- variable3[i-1]+6
  variable4[i] <- variable4[i-1]+6
  variable5[i] <- variable5[i-1]+6
  variable6[i] <- variable6[i-1]+6
  
}


scrutin_numero <- prov[variable1]
date_scrutin <- prov[variable2]
resume_legislatif_associe <- prov[variable3]


scrutin_dossier_data_prov <- data.frame(scrutin_numero,date_scrutin,resume_legislatif_associe)
scrutin_dossier_data_prov <- scrutin_dossier_data_prov[str_detect(scrutin_dossier_data_prov$resume_legislatif_associe,"dossier"),]

scrutin_dossier_data <- data.frame(scrutin_dossier_data_prov,url_dossier_associe)
#######################
dossier_scrutin <- unique(rbind(dossier_scrutin,scrutin_dossier_data))

#Fonction permettant de récupérer l'adresse du texte initale de loi à partir de l'adresse du dossier associé
fonction_url_texte_loi <- function(url){
  print(url)
  resume_loi_url <- read_html(url) %>% 
    html_nodes("a")%>% 
    html_attr('href') %>%
    as_tibble() %>%
    mutate_at("value", str_match, pattern="/dyn/15/textes/.*_proposition-loi$|/dyn/15/textes/.*_proposition-resolution$|/dyn/15/textes/.*_projet-loi$") %>%
    na.omit() %>%
    filter(value == max(value)) %>%
    mutate(value = paste0("https://www.assemblee-nationale.fr",value)) %>%
    unique()
  
  resume_loi_url <- resume_loi_url[1,1]
  resume_loi_url <- data.frame(url,resume_loi_url)
  return(resume_loi_url)
}
#Certaine page n'existe pas, cela cause un problème, 
#On s'assure donc que le fait d'avoir d'un probleme n'empeche pas la continuation de l'application de la fonction
fonction_url_texte_loi <- possibly(fonction_url_texte_loi, otherwise = FALSE)
#Application de la fonction pour chaque dossier legislatif
url_texte_loi <- lapply(unique(dossier_scrutin$url_dossier_associe),fonction_url_texte_loi)
#On supprime les fois où la fonction n'a rien récupérer
url_texte_loi <- url_texte_loi[lapply(url_texte_loi, isFALSE) == FALSE]
#On crée un data.frame
url_texte_loi <- bind_rows(na.omit(url_texte_loi))




#On s'assure du type de certaine variable
url_texte_loi$value <- as.character(url_texte_loi$value)


#Fonction permettant de récupérer l'adresse du texte de loi publié au JO à partir de l'adresse du dossier associé
fonction_url_texte_loi_JO <- function(url){
  print(url)
  resume_loi_url <- read_html(url) %>% html_nodes("a")%>% 
    html_attr('href') %>%
    as_tibble() %>%
    mutate_at("value", str_match, pattern="http://www.legifrance.gouv.fr/.*$") %>%
    na.omit() %>%
    unique() 
  
  resume_loi_url <- resume_loi_url[1,1]
  resume_loi_url <- data.frame(url,resume_loi_url)
  return(resume_loi_url)
}
#Certaine page n'existe pas, cela cause un problème, 
#On s'assure donc que le fait d'avoir d'un probleme n'empeche pas la continuation de l'application de la fonction
fonction_url_texte_loi_JO <- possibly(fonction_url_texte_loi_JO, otherwise = FALSE)
#Application de la fonction pour chaque dossier legislatif
url_texte_loi_JO <- lapply(unique(dossier_scrutin$url_dossier_associe),fonction_url_texte_loi_JO)
#On supprime les fois où la fonction n'a rien récupérer
url_texte_loi_JO <- url_texte_loi_JO[lapply(url_texte_loi_JO, isFALSE) == FALSE]
url_texte_loi_JO <- bind_rows(na.omit(url_texte_loi_JO))
#On s'assure du type de certaine variable
url_texte_loi_JO$value <- as.character(url_texte_loi_JO$value)


names(url_texte_loi)[1] <- "url_dossier_associe"
names(url_texte_loi)[2] <- "texte_loi"
names(url_texte_loi_JO)[1] <- "url_dossier_associe"
names(url_texte_loi_JO)[2] <- "texte_loi_JO"
  
data_loi <- merge(dossier_scrutin,url_texte_loi,by="url_dossier_associe",all.x = TRUE)
data_loi <- merge(data_loi,url_texte_loi_JO,by="url_dossier_associe",all.x = TRUE) %>%
  mutate(nom_loi = str_replace_all(
                   str_remove(
                   str_remove(
                   str_remove(
                   str_remove(
                   str_remove(
                   str_remove(url_dossier_associe,"https://www.assemblee-nationale.fr/15/dossiers/")
                                                 ,"https://www.assemblee-nationale.fr/dyn/14/dossiers/")
                                                 ,".asp")
                                                 ,"https://www.assemblee-nationale.fr/dyn/15/dossiers/")
                                                 ,"etape=15-AN1-DEPOT")
                                                 ,"https:www.assemblee-nationale.fr/dyn/14/dossiers/")
                                                 ,"[[:punct:]]", " ")) %>%
  mutate(nom_pdf = paste0(str_remove(texte_loi,"https://www.assemblee-nationale.fr/dyn/15/textes/"),".pdf")) %>%
  rename(uid_loi  = scrutin_numero)

fonction_telechargement_pdf <- function(url){
    print(url)
    download.file(paste0(url,".pdf"),  paste0(getwd(), "/", str_remove(url,"https://www.assemblee-nationale.fr/dyn/15/textes/")))
  }
fonction_telechargement_pdf <- possibly(fonction_telechargement_pdf, otherwise = FALSE)

setwd("C:/User/GoldentzGrahamz/OneDrive/Documents/GitHub/bureaudevote/data/data_pdf_loi")
lapply(unique(data_loi$texte_loi),fonction_telechargement_pdf)
  
nom_loi <- as.data.frame(unique(data_loi$nom_loi))
names(nom_loi)[1] <- "nom_loi"

setwd("C:/Users/GoldentzGrahamz/OneDrive/Documents/GitHub/bureaudevote/data/data_loi/")
write_csv(nom_loi,"nom_loi.csv")
write_csv(data_loi,"data_loi.csv")

###### Sous réserve d'avoir vote_final, si non il faut aller le chercher
rm(list = c("depute_plus","test","url_texte_loi","url_texte_loi_JO","vote","vote_final_v1"))

data_democratie <- merge(vote_final ,data_loi,by="uid_loi",all.x = TRUE)
setwd("C:/Users/GoldentzGrahamz/OneDrive/Documents/GitHub/bureaudevote/data/data_democratie/")
fwrite(data_democratie,"data_democratie.csv")
