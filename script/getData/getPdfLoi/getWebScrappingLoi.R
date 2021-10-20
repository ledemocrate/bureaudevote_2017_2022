library(rvest)
library(stringr)
library(tidyverse)
library(purrr)
install.packages("pdftools",dependencies = TRUE)
library(pdftools)

sequence_offset <- c(1,c(1:37)*100)

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

dossier_scrutin <- lapply(sequence_offset,function_get_url_dosier_scrutin)
dossier_scrutin <- bind_rows(dossier_scrutin)
dossier_scrutin$url_dossier_associe <- as.character(dossier_scrutin$url_dossier_associe)
dossier_scrutin$scrutin_numero <- as.numeric(dossier_scrutin)


fonction_url_texte_loi <- function(url){
  print(url)
  resume_loi_url <- read_html(url) %>% html_nodes("a")%>% 
    html_attr('href') %>%
    as_tibble() %>%
    mutate_at("value", str_match, pattern="https://www.assemblee-nationale.fr/15/propositions/.*$|https://www.assemblee-nationale.fr/15/projets/.*$") %>%
    na.omit() %>%
    unique() 
  
  resume_loi_url <- resume_loi_url[1,1]
  resume_loi_url <- data.frame(url,resume_loi_url)
  return(resume_loi_url)
}

fonction_url_texte_loi <- possibly(fonction_url_texte_loi, otherwise = FALSE)
url_texte_loi <- lapply(unique(dossier_scrutin$url_dossier_associe),fonction_url_texte_loi)
url_texte_loi <- url_texte_loi[lapply(url_texte_loi, isFALSE) == FALSE]
url_texte_loi <- bind_rows(na.omit(url_texte_loi))
url_texte_loi$value <- as.character(url_texte_loi$value)

fonction_pdf_loi <- function(url){
  print(url)
  resume_loi_url <- read_html(url) %>% html_nodes("a")%>% 
    html_attr('href') %>%
    as_tibble() %>%
    mutate_at("value", str_match, pattern="(.*?)\\.pdf$")%>%
    na.omit() %>%
    unique()
  resume_loi_url <- resume_loi_url[1,1]
  resume_loi_url <- data.frame(url,resume_loi_url)
  return(resume_loi_url)
}

fonction_pdf_loi <- possibly(fonction_pdf_loi, otherwise = FALSE)
pdf_loi <- lapply(url_texte_loi$value,fonction_pdf_loi)
pdf_loi <- pdf_loi[lapply(pdf_loi, isFALSE) == FALSE]
pdf_loi <- bind_rows(na.omit(pdf_loi))
pdf_loi$value <- str_c("https://www.assemblee-nationale.fr", pdf_loi$value[,1])

##########TELECHARGEMENT
fonction_telechargement_pdf <- function(url){
  print(url)
  download.file(url,  paste0(getwd(), "/", str_remove(url,"https://www.assemblee-nationale.fr/dyn/15/textes/")))
}

setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/data_pdf_loi")
lapply(pdf_loi$value,fonction_telechargement_pdf)
########################

names(url_texte_loi)[1] <- "url_dossier_associe"
names(url_texte_loi)[2] <- "texte_loi"
names(pdf_loi)[1] <- "texte_loi"
names(pdf_loi)[2] <- "pdf_adresse"

data_loi <- merge(unique(merge(pdf_loi,url_texte_loi,by="texte_loi")),dossier_scrutin,by="url_dossier_associe")
data_loi$scrutin_numero <- as.numeric(as.character(data_loi$scrutin_numero))
data_loi <- data_loi %>%
  mutate(nom_loi = str_remove(url_dossier_associe,
                              "https://www.assemblee-nationale.fr/15/dossiers/")) %>%
  group_by(nom_loi) %>%
  filter(scrutin_numero == min(scrutin_numero))%>%
  ungroup()

data_vote <- merge(data_loi,vote,by.x="scrutin_numero",by.y="uid_loi")

fonction_synthese_loi <- function(url){
  print(url)
  synthese_loi <- read_html(paste0("https://datan.fr/votes/legislature-15/vote_",url)) %>% 
                                html_nodes("p")%>% html_text()
  synthese_loi <- data.frame(url,synthese_loi)
                            
  return(synthese_loi)
}
fonction_synthese_loi <- possibly(fonction_synthese_loi, otherwise = FALSE)
data_resume <- lapply(data_loi$scrutin_numero,fonction_synthese_loi)


setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/data_pdf_loi")
myfiles <- list.files()

fonction_pdf_to_text <- function(url){
  print(url)
  txt_output <- pdftools::pdf_text(paste0("/home/gollentw/Documents/ScriptR/projetDemocratie/data/data_pdf_loi/",url)) %>%
    paste(sep = " ") %>%
    stringr::str_replace_all(fixed("\n"), " ") %>%
    stringr::str_replace_all(fixed("\r"), " ") %>%
    stringr::str_replace_all(fixed("\t"), " ") %>%
    stringr::str_replace_all(fixed("\""), " ") %>%
    paste(sep = " ", collapse = " ") %>%
    stringr::str_squish() %>%
    stringr::str_replace_all("- ", "") 
  return(txt_output)
}

top_100 = lexRankr::lexRank(fonction_pdf_to_text(myfiles[1]),
                          #only 1 article; repeat same docid for all of input vector
                          docId = rep(1, length(text)),
                          #return 3 sentences to mimick /u/autotldr's output
                          n = 100,
                          continuous = TRUE)

#reorder the top 3 sentences to be in order of appearance in article
order_of_appearance = order(as.integer(gsub("_","",top_100$sentenceId)))
#extract sentences in order of appearance
ordered_top_100 = top_100[order_of_appearance, "sentence"]

test <- paste(ordered_top_100)
test
