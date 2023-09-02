#
library(tidyverse)
library(viridis)
#read in data 
e001 <- read.csv("Data/data_e001herbivory_allNTrt.csv")
source("e001Herbivory_functions.R")
#make a factor
e001$NTrt <- as.factor(e001$NTrt)
levels(e001$NTrt)#check levels
#relevel 
e001$NTrt <- factor(e001$NTrt,levels(e001$NTrt)[c(2,1,3,5,7,8,9,4,6)])
#get the data frame with plot metadata 
map <- e001 %>% 
  filter(Year>2006) %>%
  group_by(Plot,NTrt,FTrt)
#read in row column assignment 
rowcol <- read.csv("Data/e001_fieldC_rowcol.csv")
#join together 
map <- left_join(map,rowcol)
#in caption
##reverse columns to have the plots face north
col2 <- data.frame(col2=c(1,2,3,4,5,6,7,8,9),col=c(9,8,7,6,5,4,3,2,1))
map <- left_join(map,col2)
row2 <- data.frame(row2=c(1,2,3,4,5,6),row=c(6,5,4,3,2,1))
map <- left_join(map,row2)
#plot n treatment in 2d space
map$NTrt <- gsub(pattern = "-g N",replacement = "",x = map$NTrt)
map$NTrt <- gsub(pattern = "0.00",replacement = "0.00-A",x = map$NTrt)
map$NTrt <- gsub(pattern = "0.00-A-I",replacement = "0.00-I",x = map$NTrt)
#
map$NTrt <- as.factor(map$NTrt)
levels(map$NTrt)
map$NTrt<- factor(map$NTrt,
                       levels(map$NTrt)[c(2,1,3,5,7,8,9,4,6)])

#get reduced data frame 
map1 <- map %>% select(row2,col2,NTrt,Plot,FTrt,NAdd) %>% 
  distinct()
#####
#plot nadd map 
####
nadd.map <- ggplot(map1,aes(x=row2,y=col2,
                           fill=NTrt,
                           label=Plot))+
  geom_tile(col="black")+
  scale_fill_viridis(discrete=TRUE,
                     option="D",
                     direction=-1,begin=0,end = 1)+
  ggtitle("b")+
  ylab("")+
  xlab("")+
  geom_text(aes(col=NAdd<9))+
  scale_color_manual(values=c("White","Black"))+
  guides(color="none")+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6))+
  theme_classic()+
  theme(legend.justification = c(0,-1),
        legend.position = "bottom",
        legend.key.height =  unit(0.05, "cm"),
        legend.key.width = unit(0.07,"cm"),
        legend.text = element_text(size=7),
        legend.box.spacing = unit(-0.4,"cm"),
        legend.box.margin = margin(0,0,0,0,"cm"),
        plot.margin = margin(0,0,0,0),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  ggtitle("b")
nadd.map
#####
#plot fencing treatment in 2d
####
fence <- ggplot(map1,aes(x=row2,y=col2,
                        col=FTrt,
                        label=Plot,
                        fill=FTrt))+
  scale_fill_brewer(palette = "Dark2")+
  geom_tile(col="Black")+
  geom_text(col="Black")+
  ylab("")+
  xlab("")+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6))+
  theme_classic()+
  theme(legend.justification = c(0,-1),
        legend.position = "bottom",
        legend.key.height =  unit(0.2, "cm"),
        legend.key.width = unit(0.5,"cm"),
        legend.text = element_text(size=8),
        legend.box.spacing = unit(-0.4,"cm"),
        legend.box.margin = margin(0,0,0,0,"cm"),
        plot.margin = margin(0,0,0,0),
        axis.ticks = element_blank(),
        axis.text = element_blank())+
  ggtitle("a")
fence
#
library(gridExtra)
ggsave(filename = "Figures/e001Herbivory_SupplementalFigureS1.pdf",
       plot = grid.arrange(fence,nadd.map,ncol=2),
       # device = cairo_ps,
       dpi=600,
       width=6,height=3)

