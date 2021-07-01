
### Probabilistic assignment to latent class after LCA was run in Stata 15. ###
### "What’s Behind a Racial Category? Uncovering Heterogeneity Among Asian Americans Through A Data-Driven Typology" ###
### Lucas G. Drouhot & Filiz Garip, August 2020 ###


library("haven")
library("tidyverse")
library("broom")
library("modelr")
library("magrittr")



path.output   <- "edit path"
path.data <- "edit path to data"
setwd(path.data)

file.name <-paste(path.data,"NAAS16_pooled_for_paper_after_LCA.dta",sep="")

NAAS16_pooled_for_paper_after_LCA <- read.dta(file.name, convert.factors = FALSE) %>% as_tibble()

posterior_probabilities <- cbind(NAAS16_pooled_for_paper_after_LCA$cpost1,
								NAAS16_pooled_for_paper_after_LCA$cpost2,
								NAAS16_pooled_for_paper_after_LCA$cpost3,
								NAAS16_pooled_for_paper_after_LCA$cpost4,
								NAAS16_pooled_for_paper_after_LCA$cpost5)

NAAS16_pooled_for_paper_after_LCA <- NAAS16_pooled_for_paper_after_LCA %>% 
mutate(probabilistic_class_assignment=Hmisc::rMultinom(posterior_probabilities,1))

probabilistic_class_assignment <- cbind(NAAS16_pooled_for_paper_after_LCA$respid,
										NAAS16_pooled_for_paper_after_LCA$probabilistic_class_assignment)

probabilistic_class_assignment <- as.data.frame(probabilistic_class_assignment)

probabilistic_class_assignment <- probabilistic_class_assignment %>% rename(respid2=V1, probabilistic_class_assignment=V2)

write_dta(probabilistic_class_assignment, "probabilistic_class_assignment.dta")

