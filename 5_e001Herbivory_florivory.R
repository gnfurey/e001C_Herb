library(tidyverse)
library(broom)
library(grid)
library(gridExtra)
source("e001Herbivory_functions.R")
#long data with all species 
e001.l <- read.csv("Data/data_e001herbivory_l.csv")
#read in herbivory data 
dat <- read.csv("Data/data_e001herbivory_herb_meta.csv")
#
dat$total <- dat$Herb+dat$Count
dat$prop <- dat$Herb/dat$total
#clean data 
dat <- dat %>%
  filter(Plot %ni% c(25,26,31,32,37)) %>% #remove plots that were browsed during the fencing failure
  filter(NTrt %ni% c("I-0.00","H-27.2","G-17.0"))#remove treatments not included
#plots that must be removed due to fencing failure that year 
# tt <- dat %>% filter(Plot %in% c(25,26,31,32,37))
#
splist <- read.csv(file = "Data/e001Herbivory_splist.csv")
splist <- splist$Species
##turn NA to zero 
dat[is.na(dat)] <- 0
#calculate mean proportion browsed
dat.m <- dat %>%
  group_by(Species) %>% 
  summarise(prop=mean(prop))%>% 
  filter(prop>0)
#####
#this function takes the amount of counts of herbivory
#and gets the sum
####
flowfun <- function(x){
  print(x)
  # x="Poaprate"
  tmp <- dat %>% filter(Species == x) %>% 
    select(Plot,Species,Fenced,Herb,total) %>% 
    group_by(Fenced) %>% 
    summarise(Herb=sum(Herb),
              total=sum(total)) %>% 
    mutate(prop=(Herb/total)*100) %>% 
    select(Fenced,prop,Herb,total) %>% 
    filter(Fenced==0)
  return(tmp)
}
#number of plots
#check number of plots
length(unique(dat[dat$Fenced==0,"Plot"]))
#name list 
names(splist) <- set_names(splist)
#run function
counts <- map_df(splist,flowfun,.id="Species")
#get groupings
counts$grassforb <- get_grassforb(counts$Species)
#####
#calculate total proportion inside and outside the fence 
####
flowfun2 <- function(x){
  # print(x)
  tmp <- dat %>% filter(Species == x) %>% 
    select(Plot,Species,Fenced,Herb,total) %>% 
    group_by(Fenced) %>% summarise(Herb=sum(Herb),
                                   total=sum(total))
  return(tmp)
}
############
#run the function
forprop <- map_dfr(.x = splist,flowfun2,.id="Species")
#take the data in a two column format and run the test 
propfun <- function(x){
  print(x)
  # x="Solirigi"
  tmp1 <- forprop %>% filter(Species == x)
  out <- prop.test(x = tmp1$Herb,n = tmp1$total)
  out
  out <- tidy(out)
  return(out)
}
#run on species with a sample greater than 5
specs1 <- c("Asteazur","Solirigi","Euphcoro","Lathveno")#greater than 5
specs1 <- set_names(specs1)
#run function
outs <- map_dfr(.x = specs1,propfun,.id="Species")
outs <- outs %>% ungroup()
outs$p.adjust <- p.adjust(outs$p.value,method = "fdr")
outs$p.adjust
outs$p.value
#join them together with the raw counts 
outjoin <- outs %>% select(Species,conf.low,conf.high)
counts <- left_join(counts,outjoin)
########
counts$Species <- get_species(counts$Species)
counts$Species <- as.character(counts$Species)
counts$Species <- ifelse(counts$Species=="Agropyron repens",
                      "Elymus repens",
                      ifelse(counts$Species=="Aster azureus",
                             "Symphyotrichum oolentangiense",counts$Species))
counts$Species <- gsub(pattern = " ",x = counts$Species,replacement=" \n ")
counts$Species <- factor(counts$Species)
#
counts
