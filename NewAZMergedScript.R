# Title: Analyzing terrestrial invertebrates in DRIPs. 
# Author details: Angie Nicolas, angie.nicolas@ubc.ca
# Script and data info: This script performs a time series analyses on count data.  
# Data consists of: insect counts from pitfall traps
# Data was collected in: MKRF BY Lenka & Zelie, summer 2017.

library(plyr)
library(ggplot2)
library(gridExtra)
library(lme4)
library(lubridate)
library(lmerTest)
library(visreg)
library(emmeans)
library(glmm)
library(dplyr)
library(vegan)

Merged<-read.csv("C:/Users/nicangie/Documents/Ang/Projects and Samples/Lenka Zelie samples/Pitfall samples/PitfallGit2R/AZMergedforR.csv")
names(Merged)
str(Merged) #is truncated, try..
str(Merged, list.len=ncol(Merged)) #to show all columns
Merged$Format.date <- dmy(as.character(Merged$Format.date)) #format date column
 
## New! Must remove flooded samples from analysis: May: Griff Dis 4 (Z69) and G Dis 1(Z58);June: G NonDis 2(A34), Griff Dis 4(A26), Griffiths NonDis 1 (A15)
# checking which row names to remove
which(Merged$ANumber == "Z69") 
# include confirm with Lenka if I should remove sample that said 'All flooded', Z77, Mike Dis 4 May, Abund 0
#Merged2 <- Merged[-c(77),] #if just based on Zelie's datasheet I got
Merged2<-Merged[-c(15,26,34,58,69,77), ] # the first five from Lenka Aug phonecall


#to count number of samples per group
with(Merged, tapply(Stream, Month, count))
#means
tapply(Merged$Abund, list(Merged$GWstatus, Merged$Month),mean, na.rm=TRUE)

#### ABUNDANCE Rough means
# 4X more insects in July!
with(Merged, tapply(Abund, Month, mean,na.rm=T)) # Mean July 431 vs June 144
##Comparably similar between Streams, higher abund in G
with(Merged, tapply(Abund, Stream, mean,na.rm=T))
##Discharge lower abundance than non-discharge
with(Merged, tapply(Abund, GWstatus, mean,na.rm=T))

#### DIVERSITY Rough means
# More taxa in July
with(Merged, tapply(Tot.Taxa, Month, mean,na.rm=T)) # 
## In G
with(Merged, tapply(Tot.Taxa, Stream, mean,na.rm=T))
## In Non Dis
with(Merged, tapply(Tot.Taxa, GWstatus, mean,na.rm=T))


################ ABUNDANCE PLOTS ###################################### 
#Abundance summary
Abundsumm<-summarySE(Merged,measurevar="Abund",groupvars=c("GWstatus","Stream","Month"))
#Note:function command is valid but for "plyr" package. If you load the "dplyr" in the same script you get "the error that you mentioned "Error: All arguments must be named".

# Preliminary plot all groups
p1<-ggplot(Abundsumm,aes(x=GWstatus,y=Abund,shape=Stream,color=Month))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=Abund-se,ymax=Abund+se),width=.3,position=position_dodge(0.2))+theme_bw()
p1+labs(title="Mean abundance in pitfall traps", x="Ground water status", y= "Mean Abundance per trap")+theme(legend.position = "none") 
#add to remove legend (for multiplots) +theme(legend.position = "none") 

#Per Month (combining streams)
Abundsumm2<-summarySE(Merged,measurevar="Abund",groupvars=c("GWstatus","Month"))
p2<-ggplot(Abundsumm2,aes(x=GWstatus,y=Abund, color=Month))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=Abund-se,ymax=Abund+se),width=.3,position=position_dodge(0.2))+theme_bw()
p2+labs(title="Mean abundance in pitfall traps, streams combined", x="Ground water status", y= "Mean Abundance")

#Per stream (months combined)
Abundsumm3<-summarySE(Merged,measurevar="Abund",groupvars=c("GWstatus","Stream"))
p3<-ggplot(Abundsumm3,aes(x=GWstatus,y=Abund, color=Stream))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=Abund-se,ymax=Abund+se),width=.3,position=position_dodge(0.2))+theme_bw()
p3+labs(title="Mean abundance in pitfall traps, months combined", x="Ground water status", y= "Mean Abundance")

#Chronological abundance plot attempts
#error
Abundsumm4<-summarySE(Merged,measurevar="Abund",groupvars=c("GWstatus","Format.date")) #don't know why it gives 1NA. Check
p7<-ggplot(Abundsumm4,aes(x=Format.date,y=Abund, color=GWstatus))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=Abund-se,ymax=Abund+se),width=.3,position=position_dodge(0.2))+theme_bw()
p7+labs(title="Abundance in time", x="Date", y= "Mean Abundance") #NA datapoint without bar

#This works though
time1<-ggplot(Merged,aes(Format.date,Abund,color=GWstatus))
time1+geom_jitter(height=2,width=3)+labs(title="Abundance in time", x="Date", y= "Raw Abundance")
#alternative
time1<-ggplot(Merged,aes(Format.date,Abund,color=GWstatus))
time1+geom_point(position="jitter",size=3)

################ DIVERSITY PLOTS ###################################### 
#Diversity summary
Divsumm<-summarySE(Merged,measurevar="Tot.Taxa",groupvars=c("GWstatus","Stream","Month"))
DivsummGW<-summarySE(Merged,measurevar="Tot.Taxa",groupvars=c("GWstatus"))

# Preliminary plot all groups
p4<-ggplot(Divsumm,aes(x=GWstatus,y=Tot.Taxa,shape=Stream,color=Month))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=Tot.Taxa-se,ymax=Tot.Taxa+se),width=.3,position=position_dodge(0.2))+theme_bw()
p4+labs(title="Mean number of taxa in pitfall traps", x="Ground water status", y= "Mean Diversity")

#Per Month (combining streams)
Divsumm2<-summarySE(Merged,measurevar="Tot.Taxa",groupvars=c("GWstatus","Month"))
p5<-ggplot(Divsumm2,aes(x=GWstatus,y=Tot.Taxa, color=Month))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=Tot.Taxa-se,ymax=Tot.Taxa+se),width=.3,position=position_dodge(0.2))+theme_bw()
p5+labs(title="Mean diversity in pitfall traps, streams combined", x="Ground water status", y= "Mean Diversity")

#Per stream (months combined)
Divsumm3<-summarySE(Merged,measurevar="Tot.Taxa",groupvars=c("GWstatus","Stream"))
p6<-ggplot(Divsumm3,aes(x=GWstatus,y=Tot.Taxa, color=Stream))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=Tot.Taxa-se,ymax=Tot.Taxa+se),width=.3,position=position_dodge(0.2))+theme_bw()
p6+labs(title="Mean diversity in pitfall traps, months combined", x="Ground water status", y= "Mean Diversity")

#Chronological Diversity
time2<-ggplot(Merged,aes(Format.date,Tot.Taxa,color=GWstatus))+geom_jitter(height=2,width=3)+labs(title="Diversity in time", x="Date", y= "Number of taxa")


###Side by side plots 
library(gridExtra)
grid.arrange(p1,p4, nrow = 1) #abund and div
grid.arrange(time1,time2, nrow = 1) #abund and div in time


#Johns Cleaner plots
AbundsummGW<-summarySE(Merged,measurevar="Abund",groupvars=c("GWstatus"))
p10<-ggplot(AbundsummGW,aes(x=GWstatus,y=Abund))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=Abund-se,ymax=Abund+se),width=.3,position=position_dodge(0.2))+theme_bw()+labs(title="Abundance in pitfall traps", x="Ground water status", y= "Mean Abundance")
DiversummGW<-summarySE(Merged,measurevar="Tot.Taxa",groupvars=c("GWstatus"))
p11<-ggplot(DiversummGW,aes(x=GWstatus,y=Tot.Taxa))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=Tot.Taxa-se,ymax=Tot.Taxa+se),width=.3,position=position_dodge(0.2))+theme_bw()+labs(title="Total taxa in pitfall traps", x="Ground water status", y= "Number of taxa")

##Lenka's plot sum per monthxGW
AbsummGWMonth<-summarySE(Merged,measurevar="Abund",groupvars=c("GWstatus","Month"))
p12<-ggplot(AbsummGWMonth,aes(x=GWstatus,y=Abund,color=Month))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=Abund-se,ymax=Abund+se),width=.3,position=position_dodge(0.2))+theme_bw()+labs(title="Abundance per GW status", x="Ground water status", y= "Mean Abundance per trap")
p12

AbsummGWDate<-summarySE(Merged,measurevar="Abund",groupvars=c("GWstatus","Format.date"))
p13<-ggplot(AbsummGWDate,aes(x=Format.date,y=Abund,color=GWstatus))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=Abund-se,ymax=Abund+se),width=.3,position=position_dodge(0.2))+theme_bw()+labs(title="Abundance in DRIPs and NONDRIPs", x="Ground water status", y= "Mean Abundance per trap")
## changed for..

#step by step Abund plot for Lenka, using ggplot stat, instead of summary function (means without error bars though)
Merged$Month2 <- factor(Merged$Month, c("May", "June", "July")) #order months chronologically
AbsummGWMonth<-summarySE(Merged,measurevar="Abund",groupvars=c("GWstatus","Month2"))
ggplot(data=Merged,aes(x=Month,y=Abund,color=GWstatus))+geom_point(aes(Month, Abund, color=GWstatus),size=3,position=position_dodge(width=.005), stat = "summary", fun.y = "mean")
#Better, with bars
ggplot(data=AbsummGWMonth,aes(x=Month2,y=Abund,color=GWstatus))+geom_point(aes(Month2, Abund, color=GWstatus),size=3,position=position_dodge(width=.2), stat = "summary", fun.y = "mean")+geom_errorbar(aes(ymin=Abund-se,ymax=Abund+se),width=.6,position=position_dodge(0.2))+theme_bw()+labs(title="", x="", y= "Mean Abundance")
#Diversity version
DivsummGWMonth<-summarySE(Merged,measurevar="Tot.Taxa",groupvars=c("GWstatus","Month2"))
ggplot(data=DivsummGWMonth,aes(x=Month2,y=Tot.Taxa,color=GWstatus))+geom_point(aes(Month2, Tot.Taxa, color=GWstatus),size=3,position=position_dodge(width=.2), stat = "summary", fun.y = "mean")+geom_errorbar(aes(ymin=Tot.Taxa-se,ymax=Tot.Taxa+se),width=.6,position=position_dodge(0.2))+theme_bw()+labs(title="", x="", y= "Mean number of taxa")


######################  MODELS

is.factor(Merged$GWstatus) #optional arraging 'control' Non Dis to be first: Merged$GWstatus <- factor(Merged$GWstatus, levels=c("NonDis","Dis"))

###ABUNDANCE

m1 <- lmer(Abund ~ GWstatus+Month + (1|Stream), data = Merged)
#Rtips
summary(m1)          # variances for random effects, fit metrics
plot(m1)             # plot of residuals against predicted values
VarCorr(m1)          # variance components for random effects  
confint(m1)          # lmer: conf. intervals for fixed effects and variances 
anova(m1, type = 1)  # lmer: test fixed effects sequentially (Type I SS), order matters
anova(m1, type = 3)  # lmer: as above but using Type III Sums of Squares, order doesn't matter

#Model on sq.root transformed Abundance, recommended for count data.


### Transformed data
Merged$SqrAbund<-sqrt(Merged$Abund)
#Quick sneakpeak
SRAbundsumm<-summarySE(Merged,measurevar="SqrAbund",groupvars=c("GWstatus","Stream","Month"))
p7<-ggplot(SRAbundsumm,aes(x=GWstatus,y=SqrAbund,shape=Stream,color=Month))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=SqrAbund-se,ymax=SqrAbund+se),width=.3,position=position_dodge(0.2))+theme_bw()
p7+labs(title="Transformed abundance in pitfall traps", x="Ground water status", y= "Sq.root Abundance per trap")+theme(legend.position = "none") 

m2 <- lmer(SqrAbund ~ GWstatus+Month + (1|Stream), data = Merged)
anova(m2) #GWstatus significant (!?)
#posthoc comparing means
emmeans(m2, list(pairwise ~ GWstatus), adjust = "tukey")

#Abundance model with interactions
m4<-lmer(SqrAbund ~ GWstatus*Month + (1|Stream), data = Merged)
plot(m4)
anova(m4)# Interaction not significant

#Trying glmms with Poisson
glm(formula, family=familytype(link=linkfunction), data=)
glmm1 <- glmer(Abund ~ GWstatus + Month +(1|Stream), family=poisson, Merged2)
drop1(glmm1,test="Chisq") #Likelihood ratio test, not sure if correct, alternatively...
# fit nested model using anova
glmm2<- glmer(Abund~ Month +(1|Stream), family=poisson, Merged2)
anova(glmm1,glmm2,test="Chisq") #apparently GW status does have sign. effect
#Note: The Likelihood Ratio Test (LRT) of fixed effects requires the models be fit with by MLE (use REML=FALSE for linear mixed models)

### DIVERSITY
m3 <- lmer(Tot.Taxa ~ GWstatus+Month + (1|Stream), data = Merged) #warning! boundary (singular) fit: see ?isSingular
anova(m3)
m5<- lmer(Tot.Taxa ~ GWstatus*Month + (1|Stream), data = Merged) #interaction not significant.
anova(m5)

#Diversity model with interactions
m5<-lmer(Tot.Taxa ~ GWstatus*Month + (1|Stream), data = Merged)
plot(m5)
anova(m5)# Interaction not significant

############ ******  Groundwelling taxa only  ****** ############

MergedTerr<-read.csv("C:/Users/nicangie/Documents/Ang/Projects and Samples/Lenka Zelie samples/Pitfall samples/PitfallGit2R/MergedAirTerr.csv")
MergedTerr$Format.date <- dmy(as.character(MergedTerr$Format.date)) #format date column
MergedTerr$ATSqrAbund<-sqrt(MergedTerr$Abund)
str(MergedTerr, list.len=ncol(MergedTerr)) #to show all columns

#Quick view. Terrstrial bug abund is NONDIS>DIS
ATAS2<-summarySE(MergedTerr,measurevar="ArtTerrAbund",groupvars=c("GWstatus"))
ATAS2

# All Terrestrial transformed abundance.
ATSqrAbundsumm<-summarySE(MergedTerr,measurevar="ATSqrAbund",groupvars=c("GWstatus","Stream","Month"))
p8<-ggplot(ATSqrAbundsumm,aes(x=GWstatus,y=ATSqrAbund,shape=Stream,color=Month))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=ATSqrAbund-se,ymax=ATSqrAbund+se),width=.3,position=position_dodge(0.2))+theme_bw()+labs(title="Terrestrial arthropod abundance", x="Ground water status", y= "Mean abundance per trap")+theme(legend.position = "none") 
#add to remove legend (for multiplots) +theme(legend.position = "none") 

# Diversity Terrestrial bugs
TerrDivsumm<-summarySE(MergedTerr,measurevar="ArtTerrTaxa",groupvars=c("GWstatus","Stream","Month"))
# Preliminary plot Terrrstrial Bugs
p9<-ggplot(TerrDivsumm,aes(x=GWstatus,y=ArtTerrTaxa,shape=Stream,color=Month))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=ArtTerrTaxa-se,ymax=ArtTerrTaxa+se),width=.3,position=position_dodge(0.2))+theme_bw()+labs(title="Terrestrial arthropod diversity", x="Ground water status", y= "Mean number of taxa")
grid.arrange(p8,p9,nrow = 1) #Terrestrial



##### Resuming Dec 3, 2019  
#############################################################################


#removing unnecessary columns to leave just terrestrial sp by site df: removing site columns, non-terrestrials and totals
names(MergedTerr)
spmatrix1<-MergedTerr[, -c(4,5,7:9,17:18,25:26,33:42)] 
#removing non terrestrials

#the tidyverse version that doesn't seem to work
drops <- c("Vespidae","Dipt.Adult","Trichop.adult","Plecopt.adult")
spmatrix1[ , !(names(spmatrix1) %in% drops)]

#removing NAs
spmatrix1[is.na(spmatrix1)] <- 0 #note some columns are int some are numeric
#rows 75,77 have zero terrestrial bugs, remove or NMDS doens't run:
spmatrix1<-spmatrix1[-c(75,77),]
#if wanted to save as .csv:
write.csv(spmatrix1,"C:/Users/nicangie/Documents/Ang/Projects and Samples/Lenka Zelie samples/Pitfall samples/PitfallGit2R/spmatrix1.csv", row.names = FALSE)

tryNMDS<-metaMDS(spmatrix1[,-c(1:4)],k=2) #only works when I remove row names
tryNMDS
stressplot(tryNMDS)
plot(tryNMDS,type="t") #all mixed

# basic plot
ordiplot(tryNMDS,type="n")
orditorp(tryNMDS,display="species",col="grey",air=0.1) #adds sp names
#also try 
orditorp(tryNMDS,display="sites",cex=0.05,air=0.01)
ordihull(tryNMDS,groups=spmatrix1$GWstatus,draw="polygon",col="grey90",
         label=FALSE)

ordihull(tryNMDS,groups=spmatrix1$GWstatus,draw="polygon",col="grey90",
         label=FALSE)

orditorp(tryNMDS,display="sites",col=c(rep("green",5),rep("blue",5)),
         air=0.01,cex=1.25)
ordiellipse(tryNMDS,groups=spmatrix1$GWstatus, display="sites", draw = "lines")

#ggplot version WORKS!! :) (https://chrischizinski.github.io/rstats/vegan-ggplot2/)
#The first step is to extract the scores (the x and y coordinates of the site (rows) and species and add the grp variable
#1-Location df
data.scores <- as.data.frame(scores(tryNMDS))
data.scores$site <- rownames(data.scores) 
data.scores$grp <- spmatrix1$GWstatus
head(data.scores)
#Species df
species.scores <- as.data.frame(scores(tryNMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)

ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=3) + # add the point markers
  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=2,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("Dis" = "blue", "NonDis" = "grey")) +
  coord_equal() +
  theme_bw()

#using chull to add polygon
grp.a <- data.scores[data.scores$grp == "Dis", ][chull(data.scores[data.scores$grp == 
                                                                   "Dis", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.b <- data.scores[data.scores$grp == "NonDis", ][chull(data.scores[data.scores$grp == 
                                                                   "NonDis", c("NMDS1", "NMDS2")]), ]  # hull values for grp B

hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b
hull.data
#plot dropping site labels verison 1
ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.30) + # add the convex hulls
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=4) + # add the point markers
  scale_colour_manual(values=c("Dis" = "blue", "NonDis" = "grey")) +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

#plot version 2 works TOO :)
ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.1) + # add the convex hulls
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.3) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=2) + # add the point markers
  scale_colour_manual(values=c("Dis" = "dodgerblue4", "NonDis" = "grey65")) +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
            axis.text.y = element_blank(), # remove y-axis text
            axis.ticks = element_blank(),  # remove axis ticks
            axis.title.x = element_text(size=18), # remove x-axis labels
            axis.title.y = element_text(size=18), # remove y-axis labels
            panel.background = element_blank(), 
            panel.grid.major = element_blank(),  #remove major-grid labels
            panel.grid.minor = element_blank(),  #remove minor-grid labels
            plot.background = element_blank())



##trying NMDS with month
data.scores$grp2 <- spmatrix1$Month



### FUNCTIONS TO LOAD!!
#Summary means
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

