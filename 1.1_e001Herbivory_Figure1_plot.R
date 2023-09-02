##########
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
#write ylab with proper notation
biolab <- expression(
  paste("Aboveground biomass"," ",
        "(", "g" %.% "m"^-2,")"))
#get mean and standard error
dat.m1 <- dat %>% 
  group_by(FTrt) %>% 
  summarise(se=my.stand(TotBio),
            TotBio=mean(TotBio))
#first figure showing main effect of fencing
p1 <- ggplot(dat.m1,aes(x=FTrt,y=TotBio,fill=FTrt))+
  geom_bar(stat="identity",
           col="Black",
  )+
  geom_errorbar(aes(ymax=TotBio+se,ymin=TotBio-se),width=0.5)+
  ylab(biolab)+
  scale_fill_brewer(name="",palette = "Dark2")+
  theme_bw(base_size =12)+
  scale_y_continuous(breaks=seq(from=0,525,by=25),
                     limits=c(0,525),
                     expand = expansion(mult=c(0,0.02)),
                     labels=every_nth(seq(from=0,525,by=25),2,inverse = TRUE))+
  geom_hline(yintercept = 0)+
  geom_segment(aes(x=1,xend=2,y=350,yend=350),linewidth=0.5)+
  geom_text(aes(x=1.5,y=350,label="*"),size=10)+
  guides(fill=guide_legend(nrow=1))+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        panel.border = element_blank(),
        legend.margin = margin(0,0,0,0),
        legend.title=element_blank(),
        legend.key.height =  unit(0.1, "cm"),
        legend.key.width = unit(0.55,"cm"),
        legend.text = element_text(size=10,face="plain"),
        legend.box.spacing = unit(-0.4,"cm"),
        legend.box.margin = margin(t = 0,r = 0,b = 0.0,l = 0,"cm"))+
  xlab("Fencing treatment")
p1
#######
#generate figure to show both N and fencing
dat.m2 <- e001 %>%
  filter(NTrt %ni% c("0.00-g N-I","17.0-g N","27.2-g N")) %>% 
  filter(Year>2004) %>% 
  group_by(NTrt,FTrt) %>% 
  summarise(se=my.stand(TotBio),
            TotBio=mean(TotBio))
#rename the treatment 
dat.m2$NTrt <- gsub(pattern = "-g N",x = dat.m2$NTrt,replacement = "")
#arrange the data frame
#nadd expression
nadd <- expression(paste("Nitrogen addition treatments",
                         " ",
                         "(", "g N" %.% "m"^-2%.%"yr"^-2, 
                         ")"))
#
b1 <- ggplot(dat.m2,aes(x=NTrt,y=TotBio,fill=FTrt))+
  geom_bar(stat="identity",
           col="Black",position = position_dodge()
  )+
  geom_errorbar(aes(ymax=TotBio+se,ymin=TotBio-se),width=0.5,
                position = position_dodge(0.9))+
  theme_bw()+
  scale_y_continuous(breaks=seq(from=0,525,by=25),
                     limits=c(0,525),
                     expand = expansion(mult=c(0,0.02)),
                     labels=every_nth(seq(from=0,525,by=25),2,inverse = TRUE))+
  ylab(biolab)+
  scale_fill_brewer(palette = "Dark2")+
  xlab(nadd)+
  guides(fill=guide_legend(nrow=1))+
  theme(axis.text.x = element_text(),
        legend.position = c(.5,.95),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        panel.border = element_blank(),
        legend.margin = margin(0.5,0.5,0.5,0.5),
        legend.title=element_blank(),
        legend.key.height =  unit(0.1, "cm"),
        legend.key.width = unit(0.55,"cm"),
        legend.text = element_text(size=10,face="plain"),
        legend.box.spacing = unit(0.1,"cm"))
b1
#
library(cowplot)
#
ggsave(filename = "Figures/e001Herbivory_Figure1_final.pdf",
       plot=plot_grid(p1,b1,ncol=2,label_size=10,
                      align = "hv",labels = c("a)","b)")),
       dpi=600,
       width=7.5,height=4,unit="in")

