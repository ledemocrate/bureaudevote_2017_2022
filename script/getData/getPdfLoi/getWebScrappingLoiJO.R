library(rvest)
library(stringr)
library(tidyverse)
library(purrr)

getwd()

sequence_offset <- c(1:37)*100

function_get_descriptif_texte_loi <- function(url){
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
    url_dossier_associe1 <- paste0(url_dossier_associe1,"?etape=15-PROM",sep="")
    url_dossier_associe <- c(url_dossier_associe,url_dossier_associe1)
  }
  
  url_dossier_associe2 <- url_dossier_associe_prov[str_detect(url_dossier_associe_prov, "^https://www.assemblee-nationale.fr/dyn/14/dossiers/")]
  if(!is_empty(url_dossier_associe2)){
    url_dossier_associe1 <- paste0(url_dossier_associe2,"?etape=15-PROM",sep="")
    url_dossier_associe <- c(url_dossier_associe,url_dossier_associe2)
  }
  
  url_dossier_associe3 <- url_dossier_associe_prov[str_detect(url_dossier_associe_prov, "^https://www.assemblee-nationale.fr/15/dossiers/")]
  if(!is_empty(url_dossier_associe3)){
    url_dossier_associe1 <- paste0(url_dossier_associe3,"?etape=15-PROM",sep="")
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



test_final <- lapply(sequence_offset,function_get_descriptif_texte_loi)
test_final <- bind_rows(test_final)
test_final$url_dossier_associe <- as.character(test_final$url_dossier_associe)

fonction_resume_loi <- function(url){
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

fonction_resume_loi <- possibly(fonction_resume_loi, otherwise = FALSE)
test_final_2 <- lapply(unique(test_final$url_dossier_associe),fonction_resume_loi)
test_final_2 <- test_final_2[lapply(test_final_2, isFALSE) == FALSE]
test_final_2 <- bind_rows(na.omit(test_final_2))
test_final_2$value <- as.character(test_final_2$value)

fonction_resume_loi_2 <- function(url){
  print(url)
  resume_loi_url <- read_html("https://www.legifrance.gouv.fr/jorf/id/JORFTEXT000039666574") %>% html_nodes("p")%>% html_text("br")
  test <- paste(resume_loi_url, sep="\n")
    write.table(test, file = "test.txt", sep = "\n",
                row.names = FALSE, col.names = FALSE)
  wri
    html_attr('href') %>%
    as_tibble() %>%
    mutate_at("value", str_match, pattern="(.*?)\\.pdf$")%>%
    na.omit() %>%
    unique()
  resume_loi_url <- resume_loi_url[1,1]
  resume_loi_url <- data.frame(url,resume_loi_url)
  return(resume_loi_url)
}

fonction_resume_loi_2 <- possibly(fonction_resume_loi_2, otherwise = FALSE)
test_final_3 <- lapply(test_final_2$value,fonction_resume_loi_2)
test_final_3 <- test_final_3[lapply(test_final_3, isFALSE) == FALSE]
test_final_3 <- bind_rows(test_final_3)
test_final_3$value <- str_c("https://www.assemblee-nationale.fr", test_final_3$value[,1])


scrutins <- read_html(as.character(test_final$url_dossier_associe[1]))
prov<- scrutins %>% html_nodes(".card.mb-4 p")%>% 
  html_text2()
test <- paste0(prov,collapse = " ")
test
View(test)
prov <- prov[-c(1:3)]
