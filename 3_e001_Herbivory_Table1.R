library(tidyverse)
#This script just merges together both model tables into one
tab1 <- readRDS("Tables/Table1_biomass.RDS")
tab2 <- readRDS("Tables/Table1_sr.RDS")
colnames(tab1)[3:4] <- paste(colnames(tab1)[3:4],"biomass",sep="_")
colnames(tab2)[3:4] <- paste(colnames(tab2)[3:4],"sr",sep="_")
tab1$parm_bio <- rownames(tab1)
tab2$parm_sr <- rownames(tab2)
tab2$numDF <- NULL
tab2$denDF <- NULL
out <- cbind(tab1,tab2)
out$`p-value_biomass` <- round(out$`p-value_biomass`,3)
out$`p-value_sr` <- round(out$`p-value_sr`,3)
out$`F-value_biomass` <- round(out$`F-value_biomass`,2)
out$`F-value_sr` <- round(out$`F-value_sr`,2)
out1 <- out[-1,]
write_csv(x = out1,file = "Tables/e001Herbivory_Table1.csv")
