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

nigeria<-subset(battleData, COUNTRY=="Nigeria")
nigeraActYr = tapply(nigeria$ACTOR1, nigeria$YEAR, actCount ) 
plot(names(nigeraActYr), nigeraActYr, ylab="Number of Unique Senders", xlab="Years",
     type="p",main="Nigeria Battles 1997-2015", pch=16)

d<-som$ACTOR1[grep("Unidentified", som$ACTOR1)]
newdata = som[!som$ACTOR1 %in% d,]
gsub("'", "", newdata$ACTOR1)
write.csv(newdata, file=paste0(pathData, "SomClean.csv"))

#sampling frame
newdata = read.csv(paste0(pathData, "SomClean.csv"))

# some cleanup
newdata$a1=char(newdata$ACTOR1) %>% trim()
newdata$a2=char(newdata$ACTOR2) %>% trim()

##### from SM can we clean these names a bit? 
########## seems like we can combine some of the subclans into clans
########## somali government related forces

# flip over dataset
orig = newdata
revOrig = orig ; revOrig$a2 = orig$a1 ; revOrig$a1 = orig$a2
tmp = rbind(orig, revOrig)
yrs=seq(min(newdata$YEAR), max(newdata$YEAR), by=1)

loadPkg('doBy')
actorDates = summaryBy(YEAR ~ a1, data=tmp, FUN=c(min, max))

# should we only look at actors involved for at least a span of five years?
actorDates$yrsActive = actorDates$YEAR.max - actorDates$YEAR.min

# get rid of some remaining Unidentified actors
actorDates = actorDates[!grepl('Unidentified', actorDates$a1),]

# list of actors by year
actorsT = lapply( yrs, function(t){
  actors = NULL
  for( ii in 1:nrow(actorDates)){
     if( t %in% actorDates$YEAR.min[ii]:actorDates$YEAR.max[ii] ) { 
      actors = append(actors, actorDates$a1[[ii]]) } }
  return(actors)
}) ; names(actorsT) = yrs

# adj mats
newdata$dv = 1 ; yVar = 'dv'
yList = lapply(2006:2014, function(ii){ # 2006 is when we have +100 actors
  actorSlice = actorsT[[char(ii)]]
  slice = newdata[ which( 
      newdata$YEAR==ii & 
      newdata$a1 %in% actorSlice &
      newdata$a2 %in% actorSlice
      ), c('a1', 'a2', yVar) ]
  adjMat = matrix(0, 
    nrow=length(actorSlice), ncol=length(actorSlice),
    dimnames=list(actorSlice,actorSlice) )
  for(r in 1:nrow(slice)){ adjMat[slice$a1[r],slice$a2[r]]=1  }
  return(adjMat)
}) ; names(yList) = yrs