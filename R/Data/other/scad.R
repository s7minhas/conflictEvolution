if(Sys.info()["user"]=="cassydorff")
{ setwd("~/Dropbox/Research/nothingbutnet/conflictEvolution/data") }

## load LA data
LAscad<-read.csv("SCAD_LA_32/SCAD_LA_32.csv")
table(LAscad$etype)

#subset only violent events
laSub<-LAscad[LAscad$etype >= 7,]
dim(laSub)
table(laSub$countryname)

#subset mexico
mexData<-laSub[laSub$countryname=="Mexico",]
#subset for only violent events with at least one death 
mexDataSub<-mexData[mexData$ndeath>0,]
#name check
length(unique(mexDataSub$actor1))

#subset Guatemala
guateData<-laSub[laSub$countryname=="Guatemala",]
#subset for only violent events with at least one death 
guateDataSub<-guateData[guateData$ndeath>0,]
#name check
length(unique(guateDataSub$actor1))

#subset honduras
honData<-laSub[laSub$countryname=="Honduras",]
#subset for only violent events with at least one death 
honDataSub<-honData[honData$ndeath>0,]
#name check
length(unique(honDataSub$actor1))

## load African data
Ascad<-read.csv("SCAD_Africa_32/SCAD_Africa_32.csv")
table(Ascad$countryname)

#south Africa
southAf<-Ascad[Ascad$countryname=="South Africa",]
#subset for only violent events with at least one death 
southAfSub<-southAf[southAf$ndeath>0,]
#name check
length(unique(southAfSub$actor1))

#Nigeria
nigeria<-Ascad[Ascad$countryname=="Nigeria",]
#subset for only violent events with at least one death 
nigeriaSub<-nigeria[nigeria$ndeath>0,]
#name check
length(unique(nigeriaSub$actor1))
