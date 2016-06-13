################################################################
## analysis paper 3 - building the network measures for the model
## updated data code (fall 2015)
## Cassy Dorff
################################################################

# setup & load all data
rm(list=ls())
setwd("/Users/cassydorff/Dropbox/Research/Dissertation/Paper 3/analysis/data/")  
library(amen)
library(network)
library(sna)
library(plyr)

#Y data 
load("matListCrime.rda") #counts
load("matListCrimeBin.rda") #binary 
load("matListCrimeOrd.rda") #ordinal

#one slice matrices
dv<-as.array(matListCrime[1:9])
dvOrd<-as.array(matListCrimeOrd[1:9])
dvBin<-as.array(matListCrimeBin[1:9])

#degree centrality (nodal covariates one shot) 
cent<-degree(dvOrd[[1]], gmode="graph")
eigenCent<-evcent(dvOrd[[1]], gmode="graph")
info<-infocent(dvOrd[[1]], gmode="graph")
be<-betweenness(dvOrd[[1]], gmode="graph")

centList<-lapply(matListCrimeOrd, degree, gmode="graph")
centListLag<-centList
centListLag[[1]]<-rep(0, 17)
centListLag[2:9]<-centList[1:8]
centListLag<-lapply(centListLag, as.matrix)

beList<-lapply(matListCrimeOrd, betweenness, gmode="graph")
beList<-lapply(beList, as.matrix)
beListLag<-beList
beListLag[2:9]<-beListLag[1:8]
beListLag<-lapply(beListLag, as.matrix)

#government binary indicator (nodal covariate)
names<-unlist(dimnames(matListCrime[[1]])[1])
gov<-data.frame(names)
gov$govI<-NA
gov[gov$names=="Federal Government",]$govI<-1
gov[gov$names=="State Government",]$govI<-1
gov[gov$names=="Municipal Government",]$govI<-1
gov[gov$names=="Military",]$govI<-1
gov[gov$names=="Federal Police",]$govI<-1
gov[gov$names=="State Police",]$govI<-1
gov[gov$names=="Municipal Police",]$govI<-1
gov[is.na(gov)]<-0

#protest count indicator
protest<-read.csv("protestNames.csv")
protestList<-split(protest, protest$region)
protestLag<-read.csv("protestNamesLag.csv")
protestLagList<-split(protestLag, protestLag$region)


#load & join population data


#combine all nodals into one array (LAGGED)
nodalList<-lapply(seq(length(centListLag)), function(i)cbind(centListLag[[i]], 
              gov$govI))
nodalList<-lapply(seq(length(nodalList)), function(i)cbind(nodalList[[i]], 
              beListLag[[i]]))
nodalList<-lapply(seq(length(nodalList)), function(i)cbind(nodalList[[i]], 
             protestLagList[[i]][,4]))

allNodal<-array(unlist(nodalList), dim=c(nrow(nodalList[[1]]), ncol(nodalList[[1]]),
           length(nodalList)), dimnames=list(rownames(dvOrd[[1]]), 
          c("centrality", "govI", "betweeness", "protestLagCount"), years))

save(allNodal, file="allNodal.rda")



