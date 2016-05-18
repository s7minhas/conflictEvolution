################################################################
## analysis paper 3 - building the networks for each year
## produces a yearly network with equal actors CRIME & GOV only
## updated data code (fall 2015)
## Cassy Dorff
################################################################

# setup & load all data  
library(plyr)
library(ggplot2)
library(xtable)
library(amen)

# event data
cleanData<-read.csv(paste0(pathData, "mexicoVioStoriesFinal.csv"))

# now load panel data
panel<-read.csv(paste0(pathData,"violentActors.csv")) #updated actor list
uniqueNames<-as.character(unique(panel$Actor.Name))
years<-seq(2005, 2013, 1)
actorYr=lapply(years, function(x) FUN=uniqueNames)
actorIDs=c("senderGroup1", "senderGroup2", "senderGroup3", "TargetGroup1", "TargetGroup2", "TargetGroup3")
keep<-c(actorIDs, "mexicanState", "year", "drop1", "directional")
cleanData<-subset(cleanData, select=c(keep))
cleanData<-cleanData[cleanData$drop1==0,] #~250 dropped
cleanData$event<-1
#save(cleanData,file="cleanDataSubset0516.rda")

## Aggregate
matListCrime=list()
for(ii in 1:length(years)){
  slice=cleanData[which(years[ii]==cleanData$year),] 
  atrs=actorYr[[ii]] 
  matListCrime[[ii]]=matrix(0,nrow=length(atrs),ncol=length(atrs),dimnames=list(atrs,atrs))
  
  for(jj in 1:nrow(slice)){
    sndrs=NULL; trgt=NULL
    sndrs=slice[jj,actorIDs]; sndrs=as.character(sndrs[!is.na(sndrs)])
    sndrs=sndrs[ which(sndrs %in% intersect(sndrs,rownames(matListCrime[[ii]]) ) ) ]
    
    matListCrime[[ii]][which(rownames(matListCrime[[ii]])==sndrs), which(colnames(matListCrime[[ii]])==sndrs)]<-1
    mat2=matListCrime[[ii]]
    
    if(length(setdiff(sndrs,rownames(matListCrime[[ii]])))==0){
      mat2=matrix(0, nrow=length(atrs), ncol=length(atrs), dimnames=list(atrs, atrs))
      if(years[ii]>=slice[jj,'year']){mat2[sndrs, sndrs]=slice[jj,'event']}
      matListCrime[[ii]]=matListCrime[[ii]] + mat2
    }
    
    print(years[ii])
  }
}

names(matListCrime)=years
for (i in 1:length(years)){
  diag(matListCrime[[i]])<-0
}

#make binary
matListCrimeBin<-matListCrime
for (i in 1:length(years)){
  matListCrimeBin[[i]] <- ifelse(matListCrime[[i]]>0, 1 , 0)
}

#make ordinal
matListCrimeOrd<-matListCrime
for (i in 1:length(years)){
  for (row in 1:nrow(matListCrime[[i]])){
    for (col in 1:ncol(matListCrime[[i]])){
      if(matListCrime[[i]][row,col]==1){matListCrimeOrd[[i]][row,col]<-1}
      if(matListCrime[[i]][row,col]==2){matListCrimeOrd[[i]][row,col]<-2}
      if(matListCrime[[i]][row,col]==3){matListCrimeOrd[[i]][row,col]<-2}
      if(matListCrime[[i]][row,col]==4){matListCrimeOrd[[i]][row,col]<-3}
      if(matListCrime[[i]][row,col]==5){matListCrimeOrd[[i]][row,col]<-3}
      if(matListCrime[[i]][row,col]==6){matListCrimeOrd[[i]][row,col]<-3}
    }}}

save(matListCrime, file="matListCrime0516.rda")
save(matListCrimeOrd, file="matListCrimeOrd0516.rda")
save(matListCrimeBin, file="matListCrimeBin0516.rda")


# PLOT EVENT COUNT DATA
# create counter
load("cleanDataSubset.rda")

cleanDataCount<-ddply(cleanData, .(year), 
                      summarise, eventCount=sum(event))

# or make a whole separate data frame
cleanDataCountYear<-ddply(cleanData, .(year), summarise, eventCount=sum(event))

dev.new(height=4, width=8)
q<-qplot(x=year, y=eventCount, data=cleanDataCount[cleanDataCount$year>=2004
           & cleanDataCount$year<2013,], geom="line", group=1)
q+theme_bw() + theme(text = element_text(size=20)
dev.off()

