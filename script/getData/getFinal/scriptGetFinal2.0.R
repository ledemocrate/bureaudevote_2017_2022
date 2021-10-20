######################################################
# Analyse des données de vote de l'assemblée française
######################################################

#INTEGRATION DES DONNEES VOTE(Json) & Deputés(Csv)

#LIBRAIRIE UTILISEE
library(jsonlite)  # Pour ouvrir fichier json
library(stringr)   # Pour manipuler les caracteres
library(tidyverse) # Pour avoir tout l'univers tidy (coutau-suisse de R)

#Petit rappel des informations techniques, cela ne mange pas de pain

sessionInfo()
packageStatus()
Sys.getenv()
getwd()

#CHARGEMENT DES DONNEES VOTE
# Endroit ou vous mettez les fichiers json en telechargeant sous le lien 
setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/data_vote/")
#Endroit ou telecharger les données
url <- "http://data.assemblee-nationale.fr/static/openData/repository/15/loi/scrutins/Scrutins_XV.json.zip"
download.file(url, destfile = basename(url))
#Fichier zip donc dezippage
unzip("Scrutins_XV.json.zip")
#L'archive ne nous sert plus à grand chose
file.remove("Scrutins_XV.json.zip")
#On se place là où les données sont (càd un niveau inférieur)
setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/data_vote/json")
#On recupere la liste des fichiers
liste_vote <- list.files()
#Fonction visant pour chaque fichier de la liste a recuperer les informations
read_data <- function(liste){
  #On lit le fichier de la liste
  vote <- fromJSON(liste, flatten=TRUE)
  
  #On récupère les données de vote CONTRE et on crée un data set
  vote_contre_intermediaire <- bind_rows(vote[["scrutin"]][["ventilationVotes"]][["organe"]][["groupes"]][["groupe"]][["vote.decompteNominatif.contres.votant"]])
  vote_contre_code <- rep(0.0,nrow(vote_contre_intermediaire))
  vote_contre <- data_frame(vote_contre_code,vote_contre_intermediaire$acteurRef)
  names(vote_contre) <- c("vote_code","depute_code")
  
  #On récupère les données de vote POUR et on crée un data set
  vote_pour_intermediaire <- bind_rows(vote[["scrutin"]][["ventilationVotes"]][["organe"]][["groupes"]][["groupe"]][["vote.decompteNominatif.pours.votant"]])
  vote_pour_code <- rep(1.0,nrow(vote_pour_intermediaire))
  vote_pour <- data_frame(vote_pour_code,vote_pour_intermediaire$acteurRef)
  names(vote_pour) <- c("vote_code","depute_code")
  
  #On récupère les données de vote ABSTENTION et on crée un data set
  vote_abstention_intermediaire <- bind_rows(vote[["scrutin"]][["ventilationVotes"]][["organe"]][["groupes"]][["groupe"]][["vote.decompteNominatif.abstentions.votant"]])
  vote_abstention_code <- rep(0.5,nrow(vote_abstention_intermediaire))
  vote_abstention <- data_frame(vote_abstention_code,vote_abstention_intermediaire$acteurRef)
  names(vote_abstention) <- c("vote_code","depute_code")
  
  #On lie les trois data.sets
  vote_1 <- rbind(vote_contre,vote_pour,vote_abstention)
  
  #On replique pour chaque vote les informations de la loi pour laquelle il vote
  uid_loi <- rep(vote[["scrutin"]][["numero"]],nrow(vote_1))
  date_vote <- rep(vote[["scrutin"]][["dateScrutin"]],nrow(vote_1))
  
  data <- data_frame(date_vote,vote_1,uid_loi)
  return(data)
}
#Application de la fonction pour chaque fichier de la liste
vote <- lapply(liste_vote, read_data)

#Creation d'un data.frame
vote <- bind_rows(vote)
#On verifie la coherence 
head(vote,1000)


test <- vote[,-1] %>%
  pivot_wider(
    names_from = uid_loi,
    values_from = vote_code
  )
#CHARGEMENT DES DONNEES DEPUTE
# Endroit ou vous mettez le fichier csv en telechargeant sous le lien 
setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/data_depute/")

#Endroit ou telecharger les données
url <- "https://www.data.gouv.fr/fr/datasets/r/092bd7bb-1543-405b-b53c-932ebb49bb8e"
download.file(url, destfile = basename(url))

#On lit le fichier
depute_plus_file <- list.files()[1]
depute_plus <- read.csv(depute_plus_file)

#On change le nom de la colonne sur laquelle on va joindre
names(depute_plus)[1] <- "depute_code"

#CROISEMENT DES DONNEES VOTE/DEPUTE++ par depute_code pour avoir des données exhaustives
vote_final <- merge(vote, depute_plus, by = "depute_code")  %>% 
  select(vote_code,uid_loi,
         nom,prenom,
         experienceDepute,job,groupeAbrev,naissance) %>%
  mutate(vote_code = as.factor(vote_code),
         nom = as.factor(nom),
         prenom = as.factor(prenom),
         experienceDepute = as.numeric(str_extract(experienceDepute,"\\b([0-9]|[1-9][0-9]|100)\\b")),
         job = as.factor(job),
         groupeAbrev = as.factor(groupeAbrev),
         naissance = as.Date(naissance),
         nom_prenom = paste(nom," ",prenom))

#On garde en mémoire le fichier

rm(list=c("depute_plus","vote","depute_plus_file","liste_vote","url","read_data"))
setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/out/final")
write.csv(vote_final,"vote_final.csv")
