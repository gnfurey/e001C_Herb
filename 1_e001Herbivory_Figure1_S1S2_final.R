source("e001Herbivory_functions.R")
library(nlme)
library(tidyverse)
library(emmeans)
#read in data 
e001 <- read.csv("Data/data_e001herbivory.csv")
#adjust factor name 
e001$FTrt <- ifelse(e001$FTrt=="Deer +","UnFenced","Fenced")
#include set of variables for test of nitrogen on meaningful ecological scale
dat <- e001 %>%
  filter(Year>2004) %>% 
  filter(NTrt %ni% c("0.00-g N-I","17.0-g N","27.2-g N"))%>% 
  arrange(Plot,Year) 
length(unique(dat$Plot))#check plots
#adjust name 
dat$NTrt <- gsub(pattern = "-g N",x = dat$NTrt,replacement = "")
sort(unique(dat$NTrt))#check names 
dat$NTrt <- as.factor(dat$NTrt)
#get variance weights 
v1 <- varIdent(form=~1|NTrt)
v2 <- varIdent(form=~1|FTrt)
#test full model 
tb3way.int <- lme(TotBio~
             FTrt*NTrt*log(Year),
           correlation=corCompSymm(),
           random=~1|Plot,method="ML",
           control=list(maxIter = 100,
                        msMaxIter=100,
                        niterEM=100,
                        msMaxEval = 100),
           weights = varComb(v1,v2),
           data=dat)
#check anova 
anova(tb3way.int,type="marginal")
tb3way.int_reml <- update(tb3way.int,method="REML")
anova(tb3way.int_reml,type="marginal")
#test nested model 
tb2way.int <- update(tb3way.int,TotBio~
                FTrt*NTrt*log(Year)-FTrt:NTrt:log(Year))
#likelihood ratio test 
anova(tb2way.int,type="marginal")
#generate low level interactions
int1_n_by_year <- update(tb3way.int,TotBio~
                 FTrt+NTrt*log(Year))
int1_ftrt_by_year <- update(tb3way.int,TotBio~
                 FTrt*log(Year)+NTrt)
int1_ftrt_by_NTrt <- update(tb3way.int,TotBio~
                              FTrt*NTrt+log(Year))
#
anova(int1_n_by_year,type="marginal")
anova(int1_ftrt_by_year,type="marginal")
anova(int1_ftrt_by_NTrt,type="marginal")
#without time interaction
base.int <- lme(TotBio~
                  FTrt*NTrt+log(Year),
                random=~1|Plot,method="ML",
                correlation=corCompSymm(),
                control=list(maxIter = 100,
                             msMaxIter=100,
                             niterEM=100,
                             msMaxEval = 100),
                weights = varComb(v1,v2),
                data=dat)
#
anova(int1_ftrt_by_NTrt,type="marginal")==anova(base.int,type="marginal")
#check for significance of interaction
base <- update(base.int,~FTrt+NTrt+log(Year))
anova(base,type="marginal")
#likelihood ratio test for two-way interactions
anova(tb3way.int,tb2way.int)#three-way
anova(base,base.int)# FTrt X NTrt
anova(base,int1_ftrt_by_year) #FTrt X Year
anova(base,int1_n_by_year)#NTrt*Year
#additional check using stepwise selection
backwiseselect <- MASS::stepAIC(tb3way.int)
anova(backwiseselect)
#####
#test with log biomass 
tb_log_1 <- lme(log(TotBio)~
                  FTrt*NTrt*Year,
                correlation=corCompSymm(),
                random=~1|Plot,method="ML",
                control=list(maxIter = 100,
                             msMaxIter=100,
                             niterEM=100,
                             msMaxEval = 100),
                data=dat)
summary(tb_log_1)
anova(tb_log_1)
plot(tb_log_1)
#check using backwards selection
base.log.int <- lme(log(TotBio)~
                  FTrt*NTrt+Year,
                random=~1|Plot,method="ML",
                correlation=corCompSymm(),
                control=list(maxIter = 100,
                             msMaxIter=100,
                             niterEM=100,
                             msMaxEval = 100),
                weights = varComb(v1,v2),
                data=dat)
base.log <- update(base.log.int,~FTrt+NTrt+Year)
anova(base.log,base.log.int)#No FTrt * NTrt 
base.log <- update(base.log,method="REML")
anova(base.log)
#####
#final model 
#get contrasts using REML
out <- update(base,method="REML")
summary(out)
anova(out,type="marginal")
#NTrt
ob1 <- emmeans(out,specs = ~ NTrt,df=29,reverse=TRUE)
ob1
#Fencing
ob2 <- emmeans(out,specs = ~ FTrt,df=29,reverse=TRUE)
ob2
#
# % decrease - In discussion first paragraph see ob2
round(((321-277)/321)*100,0)
#get contrasts 
conts <- contrast(ob1,method="pairwise", df=11)#tukey
conts
#showing comparison of fencing treatments
contrast(ob2,method="pairwise")
#get contrasts as an object 
conts1 <- fortify(as.data.frame(conts))
conts1
#load packages to print as a word doc
library(flextable)
library(officer)
#output NTrt object 
outtab <- as.data.frame(conts1)
#round data
outtab <- outtab %>% 
  mutate(across(c(estimate,SE,t.ratio),~round(.x,1)))
outtab$p.value <- round(outtab$p.value,3)
#rename 
colnames(outtab) <- 
  c("Contrast","Difference","SE","DF","T.ratio","P value")
#get word object 
outtab1 <- flextable(outtab) %>% fontsize(size=12)
set_table_properties(outtab1, width = 1, layout = "autofit")
outtab1 <- font(outtab1,fontname = "Times")
#
doc1 <- read_docx() %>% 
  body_add_par(value = "S1_Table S2", style = "Normal") %>%
  body_add_flextable(value = outtab1)
#
# print(doc1, target = "Tables/e001Herbivory_TableS2_tukey.docx")
#
#get anova table
anova.tab <- anova(out,type="marginal")
anova.tab$`F-value` <- round(anova.tab$`F-value`,1)
anova.tab$Term <- rownames(anova.tab)
anova.tab <- anova.tab %>% 
  select(Term,everything())
####
library(flextable)
library(officer)
#
anova.tab$df <- NULL
#
anova.tab$`p-value`<- ifelse(anova.tab$`p-value` ==0,
                             "<0.001",
                             ifelse(anova.tab$`p-value` <0.01,
                                    "<0.01",
                                    round(anova.tab$`p-value` ,3)))
#
anova.tab1 <- flextable(anova.tab) %>% fontsize(size=12)
set_table_properties(anova.tab1, width = 1, layout = "autofit")
anova.tab1 <- font(anova.tab1,fontname = "Times")
#
doc1 <- read_docx() %>% 
  body_add_par(value = "S1_Table S2", style = "Normal") %>%
  body_add_flextable(value = anova.tab1)
# print(doc1, target = "Tables/e001Herbivory_TableS1.docx")
# write.csv(x = anova.tab,file = 
#             "Tables/S1_Table_S1.csv")
#Table 1
tb3way.int_reml <- update(tb3way.int,method="REML")
summary(tb3way.int_reml)
anova(tb3way.int_reml,type="marginal")
# write_rds(x = anova(tb3way.int_reml,type="marginal"),file = "Tables/Table1_biomass.RDS")
