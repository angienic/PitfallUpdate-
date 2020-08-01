library (ggplot2)
library(plyr); library(dplyr)
Pitfall<-read.csv("C:/Users/nicangie/Documents/Ang/Projects and Samples/Lenka Zelie samples/Pitfall samples/ForR1.csv")
names(Pitfall)
str(Pitfall) #is truncated, try..
str(Pitfall, list.len=ncol(Pitfall)) #to show all columns
which( colnames(Pitfall)=="One.cell.fly")#to extract column name
pf<-select(Pitfall,-c(13,34,50:54,56,60,63,70,74,87,98,99,103,108,112,115,117,119,126,127,130,135,136)) #remove undesired columns (repeated like One.cell.fly;empties like Beetle21, Oribatid5, Fly12; Totals and Notes) to end up with  57 obs of 112 var
pf[,'ANumber']<-factor(pf[,'ANumber'])#to convert my ANumber from integer to factor
#Replace NAs to zeroes
pf[is.na(pf)] <- 0
#Create Site per GW variable
pf$Treatment <- paste(pf$Stream,pf$GWstatus)


#### ABUNDANCE Rough totals
# 4X more insects in July!
with(Pitfall, tapply(Abund, Month, mean,na.rm=T)) # Mean July 431 vs June 144
##Comparably similar between Streams, higher abund in G
with(Pitfall, tapply(Abund, Stream, mean,na.rm=T))
##Discharge lower abundance than non-discharge
with(Pitfall, tapply(Abund, GWstatus, mean,na.rm=T))

#######CONTINUE TUESDAY!!! CALCULATE TOTALS with ***pf, instead of pitfall df***!!!!

pf$White.Lge.poduridae<-(as.numeric(pf$White.Lge.poduridae))
pf$Sminthuridae<-(as.numeric(pf$Sminthuridae))

pf$cleanAbund<-(pf$Purple.poduridae+pf$White.Lge.poduridae+pf$Sminthuridae+pf$Beetle1+pf$Beetle2+pf$Beetle3+pf$Beetle4+pf$Beetle5+pf$Beetle6+pf$Beetle7+pf$Beetle8+
                                pf$Beetle9+pf$Beetle10+pf$Beetle11+pf$Beetle12+pf$Beetle13+pf$Beetle14+pf$Beetle15+pf$Beetle16+pf$Beetle17+pf$Beetle18+pf$Beetle19+pf$Beetle20+pf$Beetle22+pf$Beetle23+pf$Beetle24+pf$Beetle25+
                                pf$Beetle26+pf$Beetle27+pf$Beetle28+pf$Beetle29+pf$Beetle30+pf$Curculionid+pf$Dryopidae+pf$Dytiscid.larvae+
                                pf$Nondytiscid.larva+pf$Dead.fungus.beetle+pf$Unknown.beetle+pf$Opilionid1+pf$Spider1+pf$Spider2+pf$Dam.Diff.Spider+pf$Pseudoscorpion+
                                pf$Ant1+pf$Wasp1+pf$Wasp2+pf$Wasp3+pf$Wasp4+pf$Wasp5+pf$Wasp6+pf$Stripey.fly+
                                pf$Empididae+pf$Psychodid.adult+pf$Tabanid+pf$Fly1+pf$Fly2+pf$Fly3+pf$Fly4+pf$Fly5+
                                pf$Fly6+pf$Fly7+pf$Fly8+pf$Fly9+pf$Fly10+pf$Fly11+pf$Fly13+pf$Tipulidae+pf$Tipulid.larva+pf$Chiron.larva+pf$Ceratopog.larva+pf$Orange.fly+
                                pf$Psychod.larva+pf$One.cell.fly.1+pf$Dipt.larvae+pf$Unknownflies+pf$Oribatid1+pf$Oribatid2+pf$Oribatid3+pf$Oribatid4+
                                pf$Prostigmatid2+pf$Red.bright+pf$Others+pf$Millepide1+pf$Millepide2+pf$Millipede3+pf$Centipede1+pf$Centipede2+
                                pf$Hemiptera+pf$Caterpillar1+pf$Cricket+pf$Trichopt.adult+pf$Plecopt.adult+pf$Flat.snail+pf$Tower.snail+pf$Black.slug+pf$Isopod1+pf$Isopod2+pf$Flea.1+pf$Oligochaete+pf$Frog+pf$Salamander)

with(pf, tapply(cleanAbund, Month, mean,na.rm=T)) 

### ABUNDANCE MEANS
# 4X more insects in July!
with(pf, tapply(cleanAbund, Month, mean,na.rm=T)) #July 267.7083; June 119.7273 
with(pf, tapply(cleanAbund, Stream, mean,na.rm=T)) #G   220.5556  GRIF 141.7619     MIKE  190.5000 
with(pf, tapply(cleanAbund, GWstatus, mean,na.rm=T)) #DIS  159.4000; NONDIS  207.1852 

###DIVERSITY...

#Summary for plots
pfsumm<-summarySE(pf,measurevar="cleanAbund",groupvars=c("GWstatus","Stream","Month"))
# Preliminary per month
p1<-ggplot(pfsumm,aes(x=GWstatus,y=cleanAbund, group=Stream,color=Month))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=cleanAbund-se,ymax=cleanAbund+se),width=.3,position=position_dodge(0.2))+theme_bw()
p1+labs(title="Mean invertebrate abundance in pitfall traps", x="Ground water status", y= "Mean Abundance")
#per stream
p1<-ggplot(pfsumm,aes(x=GWstatus,y=cleanAbund, group=Stream,color=Month))+geom_point(size=2,position=position_dodge(0.2))+geom_errorbar(aes(ymin=cleanAbund-se,ymax=cleanAbund+se),width=.3,position=position_dodge(0.2))+theme_bw()
p1+labs(title="Mean invertebrate abundance in pitfall traps", x="Ground water status", y= "Mean Abundance")



#######################################################################
#To DO LATER:
#Tot.Abund per sample 
pf$Abund<-colSums(pf[, c(10:110)])
#Total abundance per Site/Treatment
Abund<-by(pf[,10:110],pf$Treatment,FUN=colSums)

#Mean Abund per Treatment
#Not ideal way for total abundance
pf$Abund<-pf$Purple.poduridae+pf$White.Lge.poduridae+pf$Sminthuridae+pf$Beetle1+pf$Beetle2+pf$Beetle3+pf$Beetle4+pf$Beetle5+pf$Beetle6+pf$Beetle7+pf$Beetle8+pf$Beetle9+pf$Beetle10+pf$Beetle11+pf$Beetle12+pf$Beetle13+pf$Beetle14+pf$Beetle15+pf$Beetle16+pf$Beetle17+pf$Beetle18+pf$Beetle19+pf$Beetle20+pf$Beetle22+pf$Beetle23+pf$Beetle24+pf$Beetle25+pf$Beetle26+pf$Beetle27+pf$Beetle28+pf$Beetle29+pf$Beetle30+pf$Curculionid+pf$Dryopidae+pf$Dytiscid.larvae+pf$Nondytiscid.larva+pf$Dead.fungus.beetle+pf$Unknown.beetle+pf$Opilionid1+pf$Spider1+pf$Spider2+pf$Dam.Diff.Spider+pf$Pseudoscorpion+pf$Ant1+pf$Wasp1+pf$Wasp2+pf$Wasp3+pf$Wasp4+pf$Wasp5+pf$Wasp6+pf$Stripey.fly+pf$Empididae+pf$Psychodid.adult+pf$Tabanid+pf$Fly1+pf$Fly2+pf$Fly3+pf$Fly4+pf$Fly5+pf$Fly6+pf$Fly7+pf$Fly8+pf$Fly9+pf$Fly10+pf$Fly11+pf$Fly13+pf$Tipulidae+pf$Tipulid.larva+pf$Chiron.larva+pf$Ceratopog.larva+pf$Orange.fly+pf$Psychod.larva+pf$One.cell.fly.1+pf$Dipt.larvae+pf$Unknownflies+pf$Oribatid1+pf$Oribatid2+pf$Oribatid3+pf$Oribatid4+pf$Prostigmatid2+pf$Red.bright+pf$Others+pf$Millepide1+pf$Millepide2+pf$Millipede3+pf$Centipede1+pf$Centipede2+pf$Hemiptera+pf$Caterpillar1+pf$Cricket+pf$Trichopt.adult+pf$Plecopt.adult+pf$Flat.snail+pf$Tower.snail+pf$Black.slug+pf$Isopod1+pf$Isopod2+pf$Flea.1+pf$Oligochaete+pf$Frog+pf$Salamander
#Means per stream
Table1<-with(pf, tapply(Abund, Stream, mean,na.rm=T))
round(Table1, digits = 0)
#Means per GW status
Table2<-with(pf, tapply(Abund, GWstatus, mean,na.rm=T))

# #Taxa 
#Removing Taxonomic uncertainties (sp counted as different but that I'll pool for now until confirmation) removes 5 taxa from diversity count: Beetle3+Beetle5;Beetle 23+Beetle6; Stripey.fly+Fly8;Fly10+F;y4; Wasp3+Wasp5
pf$Beetle3.5<-pf$Beetle3+pf$Beetle5
pf$Beetle6.23<-pf$Beetle6+pf$Beetle23
pf$Stripey.8<-pf$Stripey.fly+pf$Fly8
pf$Fly10.4<-pf$Fly10+pf$Fly4
pf$Wasp3.5<-pf$Wasp3+pf$Wasp5


#####CHECK!!!###########
#Richness with pooled taxa, done with SUM, change to COUNT
#try some of these i.e.count3 <- length(which(b == 3)) or pf[apply(pf>0, 1, count)>0, ] DF[apply(DF == 0, 1, sum) <= 4, ]
#with sum 
pf$LowRichness<-pf$Purple.poduridae+pf$White.Lge.poduridae+pf$Sminthuridae+pf$Beetle1+pf$Beetle2+pf$Beetle4+pf$Beetle7+pf$Beetle8+pf$Beetle9+pf$Beetle10+pf$Beetle11+pf$Beetle12+pf$Beetle13+pf$Beetle14+pf$Beetle15+pf$Beetle16+pf$Beetle17+pf$Beetle18+pf$Beetle19+pf$Beetle20+pf$Beetle22+pf$Beetle24+pf$Beetle25+pf$Beetle26+pf$Beetle27+pf$Beetle28+pf$Beetle29+pf$Beetle30+pf$Curculionid+pf$Dryopidae+pf$Dytiscid.larvae+pf$Nondytiscid.larva+pf$Dead.fungus.beetle+pf$Unknown.beetle+pf$Opilionid1+pf$Spider1+pf$Spider2+pf$Dam.Diff.Spider+pf$Pseudoscorpion+pf$Ant1+pf$Wasp1+pf$Wasp2+pf$Wasp4+pf$Wasp6+pf$Empididae+pf$Psychodid.adult+pf$Tabanid+pf$Fly1+pf$Fly2+pf$Fly3+pf$Fly5+pf$Fly6+pf$Fly7+pf$Fly9+pf$Fly11+pf$Fly13+pf$Tipulidae+pf$Tipulid.larva+pf$Chiron.larva+pf$Ceratopog.larva+pf$Orange.fly+pf$Psychod.larva+pf$One.cell.fly.1+pf$Dipt.larvae+pf$Unknownflies+pf$Oribatid1+pf$Oribatid2+pf$Oribatid3+pf$Oribatid4+pf$Prostigmatid2+pf$Red.bright+pf$Others+pf$Millepide1+pf$Millepide2+pf$Millipede3+pf$Centipede1+pf$Centipede2+pf$Hemiptera+pf$Caterpillar1+pf$Cricket+pf$Trichopt.adult+pf$Plecopt.adult+pf$Flat.snail+pf$Tower.snail+pf$Black.slug+pf$Isopod1+pf$Isopod2+pf$Flea.1+pf$Oligochaete+pf$Frog+pf$Salamander+pf$Beetle3.5+pf$Beetle6.23+pf$Stripey.8+pf$Fly10.4+pf$Wasp3.5
Table3<-with(pf, tapply(LowRichness, Stream, mean,na.rm=T))
round(Table3, digits = 0)
#fix and try some of these i.e.count3 <- length(which(b == 3)) or pf[apply(pf>0, 1, count)>0, ] DF[apply(DF == 0, 1, sum) <= 4, ]
LowRich<-apply(pf,1,function(pf[,10:110])count(x != 0)) 
apply(dat,1,function(x)sum(x != 0)) 
###
##Later see for both factors
with(pf, tapply(Abund, list(GWstatus), mean,na.rm=T)) 
#Try later
Abund<-aggregate(pf$Abund,by=list(pf$GWstatus), mean)
Abund<-by(pf[,10:110],pf$Treatment,FUN=mean)
##

#Total abund across samples
GlobalTot<- rowSums(pf[c(,10:110)])


#Average abundance per GW Status
#Total taxa per GW status
#Agrupate taxa
mutate(pf, Springtails=Purple.poduridae+ White.Lge.poduridae+Sminthuridae)
mutate(pf, Beetles=Beetle1+Beetle2+Beetle3+Beetle4+Beetle5+Beetle6+Beetle7+Beetle8+Beetle9+Beetle10+Beetle11+Beetle12+Beetle13+Beetle14+Beetle15+Beetle16+Beetle17+Beetle18+Beetle19+Beetle20+Beetle22+Beetle23+Beetle24+Beetle25+Beetle26+Beetle27+Beetle28+Beetle29+Beetle30+Curculionid+Dryopidae+Dytiscid.larvae+Nondytiscid.larva+Dead.fungus.beetle+Unknown.beetle)

#Note in dplyr we *select columns, and *filter Rows


###########FUNCTION TO LOAD

###Loading Summary function to plot means and SD
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summarized
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)

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


#Mean Abund per Treatment
#Not ideal way for total abundance
pf$Abund<-pf$Purple.poduridae+pf$White.Lge.poduridae+pf$Sminthuridae+pf$Beetle1+pf$Beetle2+pf$Beetle3+pf$Beetle4+pf$Beetle5+pf$Beetle6+pf$Beetle7+pf$Beetle8+pf$Beetle9+pf$Beetle10+pf$Beetle11+pf$Beetle12+pf$Beetle13+pf$Beetle14+pf$Beetle15+pf$Beetle16+pf$Beetle17+pf$Beetle18+pf$Beetle19+pf$Beetle20+pf$Beetle22+pf$Beetle23+pf$Beetle24+pf$Beetle25+pf$Beetle26+pf$Beetle27+pf$Beetle28+pf$Beetle29+pf$Beetle30+pf$Curculionid+pf$Dryopidae+pf$Dytiscid.larvae+pf$Nondytiscid.larva+pf$Dead.fungus.beetle+pf$Unknown.beetle+pf$Opilionid1+pf$Spider1+pf$Spider2+pf$Dam.Diff.Spider+pf$Pseudoscorpion+pf$Ant1+pf$Wasp1+pf$Wasp2+pf$Wasp3+pf$Wasp4+pf$Wasp5+pf$Wasp6+pf$Stripey.fly+pf$Empididae+pf$Psychodid.adult+pf$Tabanid+pf$Fly1+pf$Fly2+pf$Fly3+pf$Fly4+pf$Fly5+pf$Fly6+pf$Fly7+pf$Fly8+pf$Fly9+pf$Fly10+pf$Fly11+pf$Fly13+pf$Tipulidae+pf$Tipulid.larva+pf$Chiron.larva+pf$Ceratopog.larva+pf$Orange.fly+pf$Psychod.larva+pf$One.cell.fly.1+pf$Dipt.larvae+pf$Unknownflies+pf$Oribatid1+pf$Oribatid2+pf$Oribatid3+pf$Oribatid4+pf$Prostigmatid2+pf$Red.bright+pf$Others+pf$Millepide1+pf$Millepide2+pf$Millipede3+pf$Centipede1+pf$Centipede2+pf$Hemiptera+pf$Caterpillar1+pf$Cricket+pf$Trichopt.adult+pf$Plecopt.adult+pf$Flat.snail+pf$Tower.snail+pf$Black.slug+pf$Isopod1+pf$Isopod2+pf$Flea.1+pf$Oligochaete+pf$Frog+pf$Salamander
#Means per stream
Table1<-with(pf, tapply(Abund, Stream, mean,na.rm=T))
round(Table1, digits = 0)
#Means per GW status
Table2<-with(pf, tapply(Abund, GWstatus, mean,na.rm=T))

# #Taxa 
#Removing Taxonomic uncertainties (sp counted as different but that I'll pool for now until confirmation) removes 5 taxa from diversity count: Beetle3+Beetle5;Beetle 23+Beetle6; Stripey.fly+Fly8;Fly10+F;y4; Wasp3+Wasp5
pf$Beetle3.5<-pf$Beetle3+pf$Beetle5
pf$Beetle6.23<-pf$Beetle6+pf$Beetle23
pf$Stripey.8<-pf$Stripey.fly+pf$Fly8
pf$Fly10.4<-pf$Fly10+pf$Fly4
pf$Wasp3.5<-pf$Wasp3+pf$Wasp5


#####CHECK!!!###########
#Richness with pooled taxa, done with SUM, change to COUNT
#try some of these i.e.count3 <- length(which(b == 3)) or pf[apply(pf>0, 1, count)>0, ] DF[apply(DF == 0, 1, sum) <= 4, ]
#with sum 
pf$LowRichness<-pf$Purple.poduridae+pf$White.Lge.poduridae+pf$Sminthuridae+pf$Beetle1+pf$Beetle2+pf$Beetle4+pf$Beetle7+pf$Beetle8+pf$Beetle9+pf$Beetle10+pf$Beetle11+pf$Beetle12+pf$Beetle13+pf$Beetle14+pf$Beetle15+pf$Beetle16+pf$Beetle17+pf$Beetle18+pf$Beetle19+pf$Beetle20+pf$Beetle22+pf$Beetle24+pf$Beetle25+pf$Beetle26+pf$Beetle27+pf$Beetle28+pf$Beetle29+pf$Beetle30+pf$Curculionid+pf$Dryopidae+pf$Dytiscid.larvae+pf$Nondytiscid.larva+pf$Dead.fungus.beetle+pf$Unknown.beetle+pf$Opilionid1+pf$Spider1+pf$Spider2+pf$Dam.Diff.Spider+pf$Pseudoscorpion+pf$Ant1+pf$Wasp1+pf$Wasp2+pf$Wasp4+pf$Wasp6+pf$Empididae+pf$Psychodid.adult+pf$Tabanid+pf$Fly1+pf$Fly2+pf$Fly3+pf$Fly5+pf$Fly6+pf$Fly7+pf$Fly9+pf$Fly11+pf$Fly13+pf$Tipulidae+pf$Tipulid.larva+pf$Chiron.larva+pf$Ceratopog.larva+pf$Orange.fly+pf$Psychod.larva+pf$One.cell.fly.1+pf$Dipt.larvae+pf$Unknownflies+pf$Oribatid1+pf$Oribatid2+pf$Oribatid3+pf$Oribatid4+pf$Prostigmatid2+pf$Red.bright+pf$Others+pf$Millepide1+pf$Millepide2+pf$Millipede3+pf$Centipede1+pf$Centipede2+pf$Hemiptera+pf$Caterpillar1+pf$Cricket+pf$Trichopt.adult+pf$Plecopt.adult+pf$Flat.snail+pf$Tower.snail+pf$Black.slug+pf$Isopod1+pf$Isopod2+pf$Flea.1+pf$Oligochaete+pf$Frog+pf$Salamander+pf$Beetle3.5+pf$Beetle6.23+pf$Stripey.8+pf$Fly10.4+pf$Wasp3.5
Table3<-with(pf, tapply(LowRichness, Stream, mean,na.rm=T))
round(Table3, digits = 0)
#fix and try some of these i.e.count3 <- length(which(b == 3)) or pf[apply(pf>0, 1, count)>0, ] DF[apply(DF == 0, 1, sum) <= 4, ]
LowRich<-apply(pf,1,function(pf[,10:110])count(x != 0)) 
apply(dat,1,function(x)sum(x != 0)) 
###
##Later see for both factors
with(pf, tapply(Abund, list(GWstatus), mean,na.rm=T)) 
#Try later
Abund<-aggregate(pf$Abund,by=list(pf$GWstatus), mean)
Abund<-by(pf[,10:110],pf$Treatment,FUN=mean)
##

#Total abund across samples
GlobalTot<- rowSums(pf[c(,10:110)])


#Average abundance per GW Status
#Total taxa per GW status
#Agrupate taxa
mutate(pf, Springtails=Purple.poduridae+ White.Lge.poduridae+Sminthuridae)
mutate(pf, Beetles=Beetle1+Beetle2+Beetle3+Beetle4+Beetle5+Beetle6+Beetle7+Beetle8+Beetle9+Beetle10+Beetle11+Beetle12+Beetle13+Beetle14+Beetle15+Beetle16+Beetle17+Beetle18+Beetle19+Beetle20+Beetle22+Beetle23+Beetle24+Beetle25+Beetle26+Beetle27+Beetle28+Beetle29+Beetle30+Curculionid+Dryopidae+Dytiscid.larvae+Nondytiscid.larva+Dead.fungus.beetle+Unknown.beetle)

#Note in dplyr we *select columns, and *filter Rows


###########FUNCTION TO LOAD

###Loading Summary function to plot means and SD
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summarized
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)

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
