library(VennDiagram)
library(tidyverse)

setwd("/home/gollentw/Documents/ScriptR/projetDemocratie/data/out/graph")
list.files()

edge <- read.csv("edge.csv")

choix_1 <- "Abad   Damien"
choix_2 <- "Ruffin   François"

venn_data <- edge %>%
  filter( (from == choix_1 | from == choix_2) & (to == choix_1 | to == choix_2) )

grid.newpage()
draw.pairwise.venn(area1 = venn_data$nombre_loi_vote.from, 
                   area2 = venn_data$nombre_loi_vote.to, 
                   cross.area = venn_data$nombre_loi_commune, 
                   category = c(as.character(venn_data$from), as.character(venn_data$to)), 
                   lty = rep("blank",2),
                   fill = c("light blue", "pink"),
                   alpha = rep(0.5, 2),
                   cat.pos =c(0,0), 
                   cat.dist = rep(0.025, 2))

grid.newpage()
draw.pairwise.venn(area1 = venn_data$nombre_vote_commun, 
                   area2 = venn_data$nombre_loi_commune-venn_data$nombre_vote_commun,
                   cross.area = 0, 
                   category = c('Nombre de vote similaire', 'Nombre de vote non similaire'), 
                   lty = rep("blank",2),
                   fill = c("light blue", "pink"),
                   alpha = rep(0.5, 2),
                   cat.pos =c(0,0), 
                   cat.dist = rep(0.025, 2))
