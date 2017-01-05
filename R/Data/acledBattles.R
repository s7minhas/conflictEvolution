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
top5B<-names(ctyOrderB[1:5]) #somalia, DRC, Sudan, Angola, Nigeria

actCount = function(x) {
  sp=unique(x)
  total=length(sp)
  result=c(total)
  return(result)
  }
 
#look at individual countries 
som<-subset(battleData, COUNTRY=="Somalia")
somActYr = tapply(som$ACTOR1, som$YEAR, actCount ) 
plot(names(somActYr), somActYr, ylab="Number of Unique Senders", xlab="Years",
     type="p",main="Somalia Battle Actors 1997-2015", pch=16)

drc<-subset(battleData, COUNTRY=="Democratic Republic of Congo")
drcActYr = tapply(drc$ACTOR1, drc$YEAR, actCount ) 
plot(names(drcActYr), drcActYr, ylab="Number of Unique Senders", xlab="Years",
     type="p",main="DRC Battle Actors 1997-2015", pch=16)

sudan<-subset(battleData, COUNTRY=="Sudan")
sudActYr = tapply(sudan$ACTOR1, sudan$YEAR, actCount ) 
plot(names(sudActYr), sudActYr, ylab="Number of Unique Senders", xlab="Years",
     type="p",main="Sudan Battle Actors 1997-2015", pch=16)

angola<-subset(battleData, COUNTRY=="Angola")
angActYr = tapply(angola$ACTOR1, angola$YEAR, actCount ) 
plot(names(angActYr), angActYr, ylab="Number of Unique Senders", xlab="Years",
     type="p",main="Angola Battle Actors 1997-2015", pch=16)

nigeria<-subset(battleData, COUNTRY=="Nigeria")
nigeraActYr = tapply(nigeria$ACTOR1, nigeria$YEAR, actCount ) 
plot(names(nigeraActYr), nigeraActYr, ylab="Number of Unique Senders", xlab="Years",
     type="p",main="Nigeria Battle Actors 1997-2015", pch=16)


