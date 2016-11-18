if(Sys.info()["user"]=="cassydorff")
{ setwd("~/Dropbox/Research/nothingbutnet/conflictEvolution/data") }

## load LA data
LAscad<-read.csv("SCAD_LA_32/SCAD_LA_32.csv")
table(LAscad$etype)

#subset only violent events
laSub<-LAscad[LAscad$etype >= 7,]
dim(laSub)

#subset mexico
mexData<-laSub[laSub$countryname=="Mexico",]
#subset for only violent events with at least one death = 1011
mexDataSub<-mexData[mexData$ndeath>0,]
#name check
length(unique(mexDataSub$actor1))


## load African data
Ascad<-read.csv("SCAD_Africa_32/SCAD_Africa_32.csv")