#################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
#################

#load data
acledAsia<-read.csv(paste0(pathData,"ACLED-Asia-Running-file-January-to-December-2015-V2.csv"), na.strings="", stringsAsFactors=FALSE)
acledAf<-read.csv(paste0(pathData,"ACLED Version 6 All Africa 1997-2015_csv_dyadic.csv"), na.strings="", stringsAsFactors=FALSE)
acledAfMon<-read.csv(paste0(pathData,"ACLED Version 6 All Africa 1997-2015_csv_monadic.csv"), na.strings="", stringsAsFactors=FALSE)
somData<-read.csv(paste0(pathData,"Somalia-file.csv"), na.strings="", stringsAsFactors=FALSE)

#################

summ = function(x) {
  total=length(x) 
  numNA = sum(is.na(x))
  leftovers = length(x) - numNA # also could this just be sum(!is.na(x))
  #result = c(total=total, numNA = numNA, leftovers = leftovers)
  result = c(leftovers = leftovers)
  return(result)}

summaryAf = tapply(acledAf$ACTOR2, acledAf$COUNTRY, summ ) 
ctyOrder<-summaryAf[order(summaryAf, decreasing=TRUE)]
top5<-names(ctyOrder[1:5])

actCount = function(x) {
  sp=unique(x)
  total=length(sp)
  result=c(total)
  return(result)
  }
  
som<-subset(acledAf, COUNTRY=="Somalia")
somActYr = tapply(som$ACTOR1, som$YEAR, actCount ) 
plot(names(somActYr), somActYr, ylab="Number of Unique Senders", xlab="Years",
     type="p",main="Somalia 1997-2015", pch=16)

DRC<-subset(acledAf, COUNTRY=="Democratic Republic of Congo")
drcActYr = tapply(DRC$ACTOR1, DRC$YEAR, actCount ) 
plot(names(drcActYr), drcActYr, ylab="Number of Unique Senders", xlab="Years",
     type="p",main="DRC 1997-2015", pch=16)

sudan<-subset(acledAf, COUNTRY=="Sudan")
sudanActYr = tapply(sudan$ACTOR1, sudan$YEAR, actCount ) 
plot(names(sudanActYr), sudanActYr, ylab="Number of Unique Senders", xlab="Years",
     type="p",main="Sudan 1997-2015", pch=16)

nigeria<-subset(acledAf, COUNTRY=="Nigeria")
nigeriaActYr = tapply(nigeria$ACTOR1, nigeria$YEAR, actCount ) 
plot(names(nigeriaActYr), nigeriaActYr, ylab="Number of Unique Senders", xlab="Years",
     type="p",main="Nigeria 1997-2015", pch=16)

zim<-subset(acledAf, COUNTRY=="Zimbabwe")
zimActYr = tapply(zim$ACTOR1, zim$YEAR, actCount ) 
plot(names(zimActYr), zimActYr, ylab="Number of Unique Senders", xlab="Years",
     type="p",main="Zimbabwe 1997-2015", pch=16)

