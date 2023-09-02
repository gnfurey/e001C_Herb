library(tidyverse)
library(nlme)
#read in data 
e001 <- read.csv("Data/data_e001herbivory.csv")
#adjust factor name 
e001$FTrt <- ifelse(e001$FTrt=="Deer +","UnFenced","Fenced")
#functions
source("e001Herbivory_functions.R")
#get data set
dat <- e001 %>%
  filter(Year>2004) %>% 
  arrange(Plot,Year) 
#check unique Ntrt
unique(dat$NTrt)
#check year 
length(unique(dat$Year))
#rename variables 
dat$NTrt <- gsub(pattern = "-g N",x = dat$NTrt,replacement = "")
dat$NTrt <- paste("N = ",dat$NTrt,sep="")
#
dat$NTrt <- as.factor(dat$NTrt)
#get error variance weights 
v1 <- varIdent(form=~1|NTrt)
v2 <- varIdent(form=~1|FTrt)
#base model 
mod0 <- lme(sr~FTrt*NTrt*Year,
            random=~1|Plot,
            weights=varComb(v1,v2),
            correlation=corCompSymm(),
            method="ML",
            control=list(maxIter = 100,
                         msMaxIter=100,
                         niterEM=100,
                         msMaxEval = 100),
            data=dat)
#remove interaction
mod_no_int <- update(mod0,
                     ~ FTrt*NTrt*Year-FTrt:NTrt:Year)
#check anova
anova(mod_no_int)
anova(mod0)
#compare nested models
anova(mod0,mod_no_int)
#get out model with REML
final <- update(mod0,method="REML")
#check anova
anova(final)
#check marginal anova
anova(final,type="marginal")
#check summary
summary(final)
#check residuals 
plot(final)
#####
#output table
summary(final)
# write_rds(x = anova(final,type="marginal"),
#           file = "Tables/Table1_sr.RDS")
library(emmeans)
#get list of available data 
list1 <- list(NTrt= unique(dat$NTrt),
              Year=seq(from=2005,to=2019),
              FTrt=c(unique(dat$FTrt)))
#create emmeans object 
em <- emmeans(object = final, 
              specs = ~NTrt*Year*FTrt,
              var="Year",
              df=5,#n=6 at each NTrt
              type="response",
              sigmaAdjust = TRUE,
              at=list1)
#report trends in sr through time 
slopes_em <- emtrends(object = final, 
                      specs = ~Year*FTrt*NTrt,
                      var="Year",
                      adjust="bonferroni",
                                 df=5,#n=6 at each NTrt
                                 type="response",
                                 sigmaAdjust = TRUE)
slopes_em
slopes_em_d <- as.data.frame(slopes_em)
##
slopes_em_d$NTrt <- gsub(pattern = coll("N = "),
                         x = slopes_em_d$NTrt,replacement = "")
nadd <- expression(paste("Nitrogen addition treatments",
                         " ",
                         "(", "g N" %.% "m"^-2%.%"yr"^-2, 
                         ")"))
s1 <- ggplot(slopes_em_d,aes(x=NTrt,y=Year.trend,col=FTrt,fill=FTrt))+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=0.15,
                position = position_dodge(width=0.3))+
  geom_point(size=3,col="black",shape=21,
             position = position_dodge(width=0.3))+
  scale_fill_brewer(name="",palette = "Dark2")+
  scale_color_brewer(name="",palette = "Dark2")+
  theme_bw(base_size = 10)+
  xlab(nadd)+
  ylab("Linear slope of species richness \n through time (Years)")+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        panel.border = element_blank())+
  ggtitle("g)")
s1
##
#test among slopes 
contrast(slopes_em,
         method = "pairwise")
##
#test each slope versus zero
test(slopes_em,null = 0,adjust="bonferroni")
###
#Generate data for plotting
#make a data.frame 
em <- as.data.frame(em)
#get pre-treatment years 
dat.m <- e001 %>% filter(Year>2002) %>%
  arrange(Plot,Year) %>%
  group_by(Year,NTrt,FTrt) %>%
  summarise(se=my.stand(sr),sr=mean(sr))
#rename factors
dat.m$NTrt <- gsub(pattern = "-g N",x = dat.m$NTrt,replacement = "")
dat.m$NTrt <- paste("N = ",dat.m$NTrt,sep="")
#factors
dat.m$NTrt <- as.factor(dat.m$NTrt)
levels(dat.m$NTrt)
#factors
dat.m$FTrt <- as.factor(dat.m$FTrt)
levels(dat.m$FTrt)
#####
#generate plot
#the function makes a separate panel for each level of Nadd
pfun <- function(x){
  # unique(dat.m$NTrt)
  # x="N = 0.00"
  tmp <- dat.m %>% 
    filter(NTrt==x)
  tmp1 <- em %>% 
    filter(NTrt==x)
  tmp$FTrt <- as.factor(tmp$FTrt)
  tmp1$FTrt <- as.factor(tmp1$FTrt)
  tmp1$FTrt <- factor(tmp1$FTrt,levels(tmp1$FTrt)[c(2,1)])
  #
  colnames(tmp1)[4] <- "sr"
  p1<- ggplot(tmp,aes(x=Year,y=sr,fill=FTrt,col=FTrt,shape=FTrt))+
    geom_errorbar(aes(ymax=sr+se,ymin=sr-se),width=0.1)+
    geom_point(col="Black",size=2)+
    scale_shape_manual(values=c(23,21),name="")+
    geom_line(aes(y=sr),data=tmp1)+
    theme_bw()+
    scale_fill_brewer(name="",palette = "Dark2")+
    scale_color_brewer(name="",palette = "Dark2")+
    theme(legend.position = "bottom",
          plot.margin = margin(t = 0.4,l = 0.2,b = 0.4,r = 0.2),
          plot.title = element_text(size=10,family = "Helvetica",
                                    margin = margin(0,0,0,0)),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=10),
          axis.title = element_text(size=10),
          panel.grid = element_blank(),
          axis.line.y = element_line(),
          axis.line.x = element_line(),
          panel.border = element_blank(),
          legend.text = element_text(size=10,family = "Helvetica",
                                     face="plain"),
          legend.margin = margin(0,0,0,0),
          legend.key.height = unit(0.02,"cm"),
          legend.box.spacing = unit(0,"cm"))+
    geom_hline(yintercept = 0)+
    geom_vline(xintercept = 2004.5)+
    ylab("Number of Species")+
    scale_y_continuous(limits=c(0,18),expand = expansion(mult = c(0.0,0.03)))+
    scale_x_continuous(breaks=seq(from=2000,to=2018,by=3),
                       expand = expansion(mult = c(0.05,0.05)),
                       labels = c("00","03","06","09","12","15","18"))+
    xlab("Year (2003-2019)")+
    theme(axis.title = element_blank())
  p1
}
#get levels of NAdd
nuts <- unique(dat.m$NTrt)
#get names for list
nuts <- set_names(nuts)
#run function
out <- map(.x = nuts,.f = pfun)
#
p1 <- out$`N = 0.00`+
  ggtitle("a)")+
  annotate(x = 2007.5,y=17.5,geom="text",size=3,
           label=("0.00 g N"))
p1
#
legendfunc<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
#
mylegend.1<-legendfunc(p1)
p1 <- p1+
  theme(legend.position = "none")
#
p2 <- out$`N = 1.02`+
  ggtitle("b)")+
  annotate(x = 2007.5,y=17.5,geom="text",size=3,
           label=("1.02 g N"))+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p2
#
p3 <- out$`N = 2.04`+
  ggtitle("c)")+
  annotate(x = 2007.5,y=17.5,geom="text",size=3,
           label=("2.04 g N"))+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

p3
#
p4 <- out$`N = 3.40`+
  ggtitle("d)")+
  annotate(x = 2007.5,y=17.5,geom="text",size=3,
           label=("3.40 g N"))+
  theme(legend.position = "none")

p4
#
p5 <- out$`N = 5.44`+
  ggtitle("e)")+
  annotate(x = 2007.5,y=17.5,geom="text",size=3,
           label=("5.44 g N"))+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

p5
#
p6 <- out$`N = 9.52`+
  ggtitle("f)")+
  annotate(x = 2007.5,y=17.5,geom="text",size=3,
           label=("9.52 g N"))+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

p6
#######
library(gridExtra)
library(grid)
library(cowplot)
ggsave(plot = plot_grid(grid.arrange(
  plot_grid(p1,p2,p3,p4,p5,p6,nrow=2,align = "h"),
  mylegend.1,
  nrow=2,
  heights=list(unit(3.8,"in"),unit(0.2,"in")),
  padding=unit(3,"mm"),
  bottom=textGrob("Year",
                  gp=gpar(cex=1,fontsize=10, 
                          family="Helvitica",fontface="plain")),
  left=textGrob(expression(paste("Species richness",
                                 " (Species "%.%  " 0.3m"^-2,")")),
                rot=90,
  gp=gpar(cex=1,fontsize=10, fontface="plain",
          family="Helvitica"))
  ),
  s1,ncol=1,rel_heights = c(1.2,0.7),rel_widths = c(1,1.1)),
  filename = "Figures/e001Herbivory_Figure2_final.pdf",
  dpi=600,
  width=7,height=6,unit="in")
