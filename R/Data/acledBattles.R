################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
#################

battleData<-read.csv(paste0(pathData,"ACLED-Version-5-All-Africa-1997-2014_battles.csv"), na.strings="", stringsAsFactors=FALSE)

summ = function(x) {
  total=length(x) 
  numNA = sum(is.na(x))
  leftovers = length(x) - numNA # also could this just be sum(!is.na(x))
  #result = c(total=total, numNA = numNA, leftovers = leftovers)
  result = c(leftovers = leftovers)
  return(result)}

summaryBatt = tapply(battleData$ACTOR2, battleData$COUNTRY, summ ) 
ctyOrderB<-summaryBatt[order(summaryBatt, decreasing=TRUE)]
top5B<-names(ctyOrderB[1:5])

actCount = function(x) {
  sp=unique(x)
  total=length(sp)
  result=c(total)
  return(result)
  }
  
som<-subset(battleData, COUNTRY=="Somalia")
somActYr = tapply(som$ACTOR1, som$YEAR, actCount ) 
plot(names(somActYr), somActYr, ylab="Number of Unique Senders", xlab="Years",
     type="p",main="Somalia Battles 1997-2015", pch=16)

d<-som$ACTOR1[grep("Unidentified", som$ACTOR1)]
gsub("'", "", newdata$ACTOR1)
newdata = som[!som$ACTOR1 %in% d,]
write.csv(newdata, file=paste0(pathData, "SomClean.csv"))

#sampling frame
orig = newdata
revOrig = orig ; revOrig$ACTOR2 = orig$ACTOR1 ; revOrig$ACTOR1 = orig$ACTOR2
tmp = rbind(orig, revOrig)

library(doBy)
actorDates = summaryBy(YEAR ~ ACTOR1, data=tmp, FUN=c(min, max) )
actorsT = lapply( yrs, 
  function(t){
  actors = NULL
  for( ii in 1:nrow(actorDates)){
     if( t %in% actorDates$YEAR.min[ii]:actorDates$YEAR.max[ii] )
     {actors = append(actors, actorDates$ACTOR1[ii]) }
return(actors)
}
})
