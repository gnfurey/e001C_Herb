library(tidyverse)
library(grid)
library(gridExtra)
library(nlme)
library(emmeans)
library(purrr)
source("e001Herbivory_functions.R")
e001.l <- read.csv("Data/data_e001herbivory_l.csv")
#####
#create labels
yylab <- expression(paste(atop("Effect of unFenced - fenced",paste("on species' biomass"," (", "g"%.%"m"^-2,")"))))
xxlab <- expression(paste("Effect of adding 1 ","g"%.%"m"^-2," N"," on species' biomass", " (", "g" %.% "m"^-2,")"))
#####
#in the methods we describe transient dynamics
#here I show what we believe is 
#evidence of transient dynamics
#in response to the fencing treatment
#so there are spillover years that need to removed
#to show the experiments effects 
e001 <- read.csv("Data/data_e001herbivory.csv")
e001$Fenced <- as.factor(e001$Fenced)
#lathveno the species we know that is eaten by deer
#it has carry over biomass for a couple of years in
#the plots that were unfenced.
#including the first couple of years obscures
#what actually happened in this experiment
# ggplot(e001,aes(x=Year,y=Lathveno,col=Fenced))+
#   stat_summary()+
#   geom_vline(xintercept = 2004)+
#   geom_vline(xintercept = 2006.5)
#function to examine plot by plot
#
###
# plotfun <- function(x){
#   tmp <- e001 %>% filter(Year>1998) %>% 
#     filter(Plot==x)
#   p1 <- ggplot(tmp,aes(x=Year,y=Lathveno,col=Fenced))+
#   geom_point(size=5)+
#   scale_color_brewer(palette = "Dark2")+
#   geom_line(col="Black")+
#   geom_vline(xintercept = 2004)+
#   geom_vline(xintercept = 2006.5)+
#   facet_wrap(~Plot,scales="free")
#   return(p1)
# }
# plist <- unique(e001$Plot)
# out <- map(.x = plist,.f = plotfun)
# ggsave(
#   filename = "Figures/transiet.tmp.pdf", 
#   plot = marrangeGrob(out, nrow=1, ncol=1), 
#   width = 7, height = 5
# )
######
#get species ranks
e001.l$func <- get_funcgroup(e001.l$Specid)
#get ranks and include functional groups 
ranks <- e001.l %>%
  filter(func %in% c("C4","C3","L","F")) %>% 
  filter(Year>2006) %>%
  group_by(Specid) %>%
  summarise(Biomass=mean(Biomass)) %>%
  arrange(desc(Biomass)) %>% 
  mutate(rank=1:n()) %>% 
  filter(rank<11)
#print ranks 
ranks
#get species list 
splist <- unique(ranks$Specid)
splist
#get species list 
outspec <- data.frame(Species=splist)
# write.csv(x = outspec,file = "Data/e001Herbivory_splist.csv",row.names = FALSE)
#demonstrate the proportion of biomass
test <- e001.l %>% 
  filter(Year>2006) %>%
  filter(Specid %in% splist) %>% 
  group_by(Specid) %>% 
  summarise(Biomass=mean(Biomass)) %>% 
  ungroup() %>% 
  summarise(sum=sum(Biomass))
#
test1 <- e001.l %>% 
  filter(Year>2006) %>%
  summarise(TotBio=mean(TotBio))
#in methods
test/test1#88% of biomass represented 
#####
#format data table
dat <- e001.l %>% 
  filter(Year>2006)
#make facotr
dat$NTrt <- factor(dat$NTrt)
#check length of years
unique(dat$Year)
length(unique(dat$Year))
unique(dat$NTrt)
#####
#the following function 
#takes each species and runs a separate regression
#it then outputs the model summary into a table
#function for each species to get effect sizes of fencing and N
diff1 <- function(x){
  # x="Agrorepe"
  #subset data
  # tmp <- tmp %>% arrange(Plot,Year)
  #######
  #General notes to future self/others
  #Modeling the individual species biomass
  #is very difficult as there are many zeroes
  #The results of these models match what we can see in the field
  #In the future, a hurdle model
  #that uses both a binomial and log/gamma distribution
  #may better fit the data
  #as of the time of analyses those two-step distributions are not well-studied 
  #for continuous data in plant ecology
  #######
  tmp <- dat[dat$Specid==x,]
  table(tmp$Year)
  unique(tmp$NTrt)
  print(x)#print species
  #get parameters
  #get variance functions 
  v1 <- varIdent(form=~1|NTrt)
  v2 <- varIdent(form=~1|FTrt)
  #some species have zero biomass at high N and this causes the model to be unstable
  if(x %in% c("Solirigi","Asteazur","Euphcoro",
                "Schiscop","Sorgnuta")){
      v1 <- varIdent(form=~1|NTrt,
                     fixed = c(
                       "9.52-g N"=1,
                       "5.44-g N"=1,
                       "3.40-g N"=1))
      v1 <- Initialize(v1,data=tmp)
      # str(v1)
  }
  parms <- c("NAdd","FTrt","log(Year)")
  fmla <- as.formula(paste("Biomass ~ ", paste(parms, collapse= "+")))
  #https://stackoverflow.com/questions/7666807/anova-test-fails-on-lme-fits-created-with-pasted-formula/7668846#7668846
  fit.bio <- do.call("lme", args = list(fmla,
                                      weights=varComb(v1,v2),
                                      control=list(maxIter = 10000,
                                                   msMaxIter=10000,
                                                   niterEM=10000,
                                                   msMaxEval = 10000),
                                      random=~1|Plot,
                                      data=tmp))
  # fit.bio=lme(fmla,
  #               weights=varComb(v1,v2),
  #             control=list(maxIter = 10000,
  #                          msMaxIter=10000,
  #                          niterEM=10000,
  #                          msMaxEval = 10000),
  #             random=~1|Plot,
  #               data=tmp)
  summary(fit.bio)
  #print out coefficients
  out1 <- matrix(nrow=1,ncol=5)
  out1[1,] <- summary(fit.bio)$tTable[2,]
  out1 <- as.data.frame(out1)
  colnames(out1) <- c("NAdd","NAdd_se","NAdd_df","NAdd_t.ratio","NAdd_p.value")
  #get the differences of fencing 
  ob2 <- emmeans(fit.bio,~FTrt,df=33)
  one=c(-1,1)
  out2 <- as.data.frame(contrast(ob2,method=list(diffN=one)))
  summary(fit.bio)$tTable[3,]
  out2
  #
  colnames(out2) <- c("cont1","Fenced","fenced_se","fenced_df","fenced_t.ratio","fenced_p.value")
  out3 <- cbind(out1,out2)
  return(out3)
}
#get species names
names(splist) <- set_names(splist) 
splist
#run function 
#it will print each species name as it runs
dat1 <- map_df(.x = splist,.f=diff1,.id = "Specid")
# write_rds(x = dat1,file = "Data/e001Herb_model.RDS")
#get grass or forb 
dat1$grassforb <- get_grassforb(dat1$Specid)
#####
#subset the data to exclude grasses 
forbs <- dat1[dat1$grassforb=="Dicot",]
forbs
#run major axis regression
mod1 <- smatr::sma(formula = Fenced~NAdd,
                     data=forbs)
mod1
r2=round(mod1$r2[[1]],2)
pval=round(mod1$pval[[1]],3)
coef <- mod1$coef[[1]]
#rename variable 
dat1$grassforb <- ifelse(dat1$grassforb=="Mono",
                           "Grasses",
                           "Forbs and Legumes")
#reorder data frame
dat1 <- dat1 %>% select(Specid,NAdd_p.value,fenced_p.value,everything())
#####
#the following cleans up the species names for printing 
#the final figure 
#get full latin binomial 
dat1$Specid <- get_species(dat1$Specid)
#
forbs$Specid <- as.character(forbs$Specid)
#
#get species names
forbs$Species <- get_species(forbs$Specid)
forbs$Species <- ifelse(forbs$Species=="Aster azureus","Symphyotrichum oolentangiense",forbs$Species)
Specid1 <- str_split(forbs$Species,pattern=" ",simplify = TRUE)
Specid1
Specid_first <- substring(text = Specid1[,1],1,1)
Specid_first <- paste(Specid_first,".",sep="")
Specid_first
Specid_both <- paste(Specid_first,Specid1[,2],sep="")
forbs$Specid
forbs$Specid1 <- Specid_both
#
forbs$Specid1
###
xxlab <- expression(paste("Effect of adding N ","on species' biomass", " (", "g" %.% "m"^-2,")"))
#####
axis.title1 <- 7
#print first panel of the figure 
o1 <- ggplot(forbs,aes(x=NAdd,y=Fenced,fill=Specid1,col=Specid1))+
  geom_hline(linetype=2,yintercept = 0)+
  geom_vline(linetype=2,xintercept = 0)+
  geom_errorbar(aes(ymax=Fenced+fenced_se,ymin=Fenced-fenced_se),
                  width=0,linewidth=0.5,col="black")+
  geom_errorbarh(aes(xmax=NAdd+NAdd_se,xmin=NAdd-NAdd_se),
                   height=0,linewidth=0.5,col="black")+
  scale_x_continuous(breaks=c(-5,0,5,10,15),limits=c(-7,17),
                     expand = expansion(mult=c(0.01,0.01)))+
  scale_y_continuous(breaks=c(-40,-20,0,20,40),limits=c(-45,47),
                     expand = expansion(mult=c(0.01,0.01)))+
  scale_fill_manual(name="",
                      values=c("Green","Grey","White","Pink","Skyblue","Yellow"))+
  scale_color_manual(name="",
                    values=c("Pink","Skyblue"))+
  geom_abline(slope=coef[2,1],intercept = coef[1,1])+
  guides(color=FALSE)+
  geom_point(size=1,shape=21,
             col="black",
             data=forbs[forbs$Specid %ni% 
                                            c("Lathveno","Asteazur"),])+
  geom_point(size=1,shape=1,data=forbs[forbs$Specid %in% 
                                          c("Lathveno","Asteazur"),])+
  theme_bw(base_size = 9)+
  theme(
      legend.position = c(0.7,0.245),
      legend.key.height =  unit(0.095, "cm"),
      legend.key.width = unit(0,"cm"),
      panel.grid = element_blank(),
      axis.line.y = element_line(),
      axis.line.x = element_line(),
      panel.border = element_blank(),
      legend.spacing.y = unit(0.05,"cm"), 
      legend.box.spacing = unit(-0.22,"cm"),
      legend.text = element_text(size=axis.title1,
                                     family = "Helvetica"),
      legend.title = element_blank(),
      legend.box.margin = margin(0,0,0,0,"cm"),
    axis.title  = element_blank())+
    xlab(xxlab)+
    ylab(yylab)+
    annotate(geom = "text",size=3,x = -4,y=40,label=paste(expression(italic("r"^2)),"==",r2),parse=TRUE)+
    annotate(geom = "text",size=3,x = -4,y=45,label=paste(expression(italic("P")),"==",pval),parse=TRUE)
o1  
#####
#get grass effects
####
#subset grasses
grass <- dat1[dat1$grassforb=="Grasses",]
#change name to updated binomial 
grass$Specid <- ifelse(grass$Specid=="Agropyron repens","Elymus repens",grass$Specid)
#
Specid1 <- str_split(grass$Specid,pattern=" ",simplify = TRUE)
Specid1
Specid_first <- substring(text = Specid1[,1],1,1)
Specid_first <- paste(Specid_first,".",sep="")
Specid_first
Specid_both <- paste(Specid_first,Specid1[,2],sep="")
grass$Specid1 <- Specid_both
#####
#generate second panel
####
o2 <- ggplot(grass,aes(x=NAdd,y=Fenced,fill=Specid1))+
  geom_hline(linetype=2,yintercept = 0)+
  geom_vline(linetype=2,xintercept = 0)+
  scale_x_continuous(breaks=c(-5,0,5,10,15),limits=c(-7,17),
                     expand = expansion(mult=c(0.01,0.01)))+
  scale_y_continuous(breaks=c(-40,-20,0,20,40),limits=c(-45,47),
                     expand = expansion(mult=c(0.01,0.01)))+
  geom_errorbar(aes(ymax=Fenced+fenced_se,ymin=Fenced-fenced_se),
                  width=0,linewidth=0.5)+
  geom_errorbarh(aes(xmax=NAdd+NAdd_se,xmin=NAdd-NAdd_se),
                   height=0,linewidth=0.5)+
  geom_point(size=1,shape=21)+
  theme_bw(base_size = 9)+
  scale_fill_brewer(palette = "Dark2",name="")+
  theme(
    legend.position = c(0.75,0.8),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.key.height =  unit(0.1, "cm"),
    legend.key.width = unit(0,"cm"),
    panel.grid = element_blank(),
    axis.line.y = element_line(),
    axis.line.x = element_line(),
    panel.border = element_blank(),
    legend.text = element_text(size=axis.title1,
                                   family = "Helvetica"),
    legend.box.spacing = unit(0,"cm"),
    legend.box.margin = margin(0,0,0,0,"cm"),
    axis.title.x  = element_blank())+
    xlab(xxlab)+
    ylab("Effect of UnFenced - \nFenced Biomass in grams")
o2
#
o1 <- o1+ggtitle("a)")+
  theme(plot.title = element_text(size=9,family = "Helvetica",
                            margin = margin(0,0,0,0)))
o2 <- o2+ggtitle("b)")+
  theme(plot.title = element_text(size=9,family = "Helvetica",
                                  margin = margin(0,0,0,0)))
library(cowplot)
pic1 <- ggdraw() +
  draw_image(magick::image_read("Figures/2016-08-11 11.36.50_original_cropped_line.JPG", 
                                density = 1200))
# pic1
###
ggsave(plot=
         plot_grid(
         arrangeGrob(plot_grid(o1,o2,ncol=2,
                        align = "h"),
                     left=textGrob(yylab,rot=90,
                                   gp=gpar(cex=1,fontsize=axis.title1,
                                           family="Helvitica",fontface="plain")),
                     bottom=textGrob(xxlab,vjust=-0.5,
                                     gp=gpar(cex=1,fontsize=axis.title1,
                                             family="Helvitica",fontface="plain"))
                     ),
         pic1,nrow=2,labels = c("","c)"),
         label_fontface = "plain",
         label_x = 0.124,
         label_y =1.08,
         label_size = 9),
       filename = "Figures/e001Herbivory_Figure3_final.jpeg",
       dpi=1200,
       # device = cairo_ps,
       width=126,height=107.5,units = "mm")
#####
#create table with effect sizes
####
#adjust p values 
dat1$NAdd_p.value.adj <- p.adjust(dat1$NAdd_p.value,method="fdr")
dat1$fenced_p.value.adj <- p.adjust(dat1$fenced_p.value,method="fdr")
colnames(dat1)
#rearrange data table 
dat2 <- dat1 %>% select(Specid,NAdd,NAdd_se,NAdd_t.ratio,NAdd_p.value,NAdd_p.value.adj,
                          Fenced,fenced_se,fenced_t.ratio,fenced_p.value,fenced_p.value.adj)
rndfun <- function(x){
    ifelse(abs(x)>0.01,round(as.numeric(as.character(x)),4),
           formatC(x,format="E",digits=1))
}
#round effects 
dat2[,c(5,6,10,11)] <- apply(dat2[,c(5,6,10,11)],2,rndfun)
dat2[,c(5,6,10,11)] <- apply(dat2[,c(5,6,10,11)],2,
                               function(x) ifelse(x<0.001,"<0.001",x))
colnames(dat2)
dat2[,c(2,3,7,8)] <-apply(dat2[,c(2,3,7,8)],2,function(x)round(x,2))
#rearrange data table
out <- dat2 %>% arrange(fenced_p.value.adj)
#convert variables to numeric 
out$Fenced <- as.numeric(as.character(out$Fenced))
out$fenced_p.value <- as.numeric(as.character(out$fenced_p.value))
out$fenced_p.value <- as.numeric(as.character(out$fenced_p.value))
#reorganize data table 
out <- out %>% select(Specid,everything()) %>% ungroup() %>% 
  arrange(fenced_p.value)
#rename columbs 
colnames(out)
out <- out %>% select(-c(NAdd_t.ratio,fenced_t.ratio))
colnames(out)
colnames(out)[2:9] <- c("Nitrogen","Nitrogen SE",
                        "Nitrogen p-value","Nitrogen p-value (FDR)",
                        "Fencing","Fencing SE",
                        "Fencing p-value","Fencing p-value (FDR)")
out$Specid <- ifelse(out$Specid=="Aster azureus","Symphyotrichum oolentangiense",out$Specid)
out$Specid <- ifelse(out$Specid=="Agropyron repens","Elymus repens",out$Specid)
#arrange by species
out <- out %>% arrange(Specid)
#####
#output table 
####
library(flextable)
library(officer)
out1 <- flextable(out) %>% fontsize(size=12)
set_table_properties(out1, width = 1, layout = "autofit")
out1 <- font(out1,fontname = "Times")
doc <- read_docx() %>% 
  body_add_par(value = "Supplemental Table x", style = "Normal") %>%
  body_add_flextable(value = out1)
# print(doc, target = "Tables/e001Herbivory_TableS3_final.docx")
#####
unique(dat$NTrt)
tmp <- dat %>% filter(NTrt=="0.00-g N")
#report relative amounts compared to total biomass
paste(out$Specid,out$Fencing/mean(tmp$TotBio)*100)
#####
#arteludo and solirigi means %
#in abstract
abstract <- dat %>% 
  filter(Specid %in% c("Arteludo","Solirigi")) %>% 
  group_by(Specid,FTrt) %>% 
  summarise(Biomass=mean(Biomass))
abstract
round(((92.4-47.6)/47.6)*100,0)
round(((9.58-44.9)/44.9)*100,0)
#