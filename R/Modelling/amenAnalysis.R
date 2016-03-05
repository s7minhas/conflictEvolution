################################################################
## analysis paper 3 - running the Amen analysis
## updated data code (fall 2015)
## Cassy Dorff
################################################################

# setup & load all data
rm(list=ls())
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
    setwd('~/Dropbox/Research/conflictEvolutionMex/data/')
} else {
    setwd("/Users/cassydorff/Dropbox/Research/Dissertation/Paper 3/analysis/data/")  
}
library(plyr)
library(ggplot2)
library(xtable)
library(amen)
library(abind)
library(amen)
library(network)

#data for DV
load("matListCrime.rda")
load("matListCrimeBin.rda")
load("matListCrimeOrd.rda")

#data for Nodal covariates
load("allNodal.rda")
load("allNodalLag.rda")

#one slice matrices
dv<-as.array(matListCrime[1:9])
dvOrd<-as.array(matListCrimeOrd[1:9])
dvBin<-as.array(matListCrimeBin[1:9])

#plot
Y1<-dv$`2005`
Y2<-dv$`2008`
Yord<-dvOrd[[1]]
YBin<-dvBin[[2]]

X<-xnet(dvOrd[[1]])
netplot(dvOrd[[1]],X)
addlines(dvOrd[[1]], X, col=Y[Y!=0])
addlines(dvOrd[[1]], X, col="lightblue", alength=0)
circplot(dvOrd[[1]], U = NULL, V = NULL, row.names = rownames(dvOrd[[1]]),
         col.names = colnames(Y), plotnames = TRUE, vscale = 0.8,
         pscale = 1.75, lcol = "gray", rcol = "brown", ccol = "blue",
         pch = 16, lty = 10, jitter = 1 * (nrow(Y)/(1 + nrow(Y))), bty = "n",
         add = FALSE)

#null model one shot: 2006
mod<-ame(Y, symmetric=TRUE, burn=1000)
modOrd<-ame(Yord, symmetric=TRUE, burn=1100, model="ord")
modBin<-ame(YBin, symmetric=TRUE, burn=1100, model="bin") 

#homogenous actors across time & space/temporal model 
#make Y
years<-seq(2005, 2013, 1)
first<-colnames(matListCrimeOrd[[1]])
second<-rownames(matListCrimeOrd[[1]])
third<-as.character(years)
allNames<-list(first,second,third)
dvMats<-array(unlist(matListCrimeBin), dim=c(nrow(matListCrimeBin[[1]]), ncol(matListCrimeBin[[1]]), 
          length(matListCrimeBin)), dimnames=allNames)
dvMatsOrd<-array(unlist(matListCrimeOrd), dim=c(nrow(matListCrimeOrd[[1]]), ncol(matListCrimeOrd[[1]]), 
          length(matListCrimeOrd)), dimnames=allNames)

#load nodal covariates 
yBin<-dvMats[,,2:8] #2006-2012
yOrd<-dvMatsOrd
Xnode<-allNodal
fitBin<-ame_rep(yBin, Xdyad=NULL, Xnode, model="bin",symmetric=TRUE,
                burn=1000,nscan=70000, odens=100)

fitOrd<-ame_rep(yOrd, Xdyad=NULL, Xnode, model="ord", symmetric=TRUE,
                burn=1000,nscan=70000, odens=100)

summary(fitOrd)
setwd('~/Desktop/cevoResultsDesktop')
save(fitBin, file="fitBin.rda")
save(fitOrd, file="fitOrd.rda")

#latent quick plots
color<-c("cadetblue4", "cadetblue4", "cadetblue4", "blue", "blue", "cadetblue4", "cadetblue4", "cadetblue4", "cadetblue4", 
         "blue", "blue", "blue","blue","blue","blue","blue","blue")
foo<-cbind(allNodal[,2,1], color)
circplot(fitOrd$EZ[,,1], U = NULL, V = NULL, row.names = rownames(fitOrd$EZ[,,1]),
         col.names = rownames(fitOrd$EZ[,,1]), plotnames = TRUE, vscale = 0.8,
         pscale = 1.75, lcol = "gray", rcol = foo[,2], ccol = foo$color,
         pch = 16, lty = 10, 
         jitter = 7 * (nrow(fitOrd$EZ[,,1])/(1 + nrow(fitOrd$EZ[,,1]))), 
         bty = "n",
         add = FALSE) 
text(-1.5,-2,"2006", lty=4)


#colors for actors
circplot(fitOrd$EZ[,,5], U = NULL, V = NULL, row.names = rownames(fitOrd$EZ[,,2]),
         col.names = rownames(fitOrd$EZ[,,1]), plotnames = TRUE, vscale = 0.8,
         pscale = 1.75, lcol = "gray", rcol = foo[,2], ccol = "blue",
         pch = 16, lty = 10, 
         jitter = 7 * (nrow(fitOrd$EZ[,,1])/(1 + nrow(fitOrd$EZ[,,1]))), 
         bty = "n",
         add = FALSE)
text(-1,2,"2010", lty=4)


