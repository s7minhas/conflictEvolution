#################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
#################

#load data
acledAsia<-read.csv(paste0(pathData,"ACLED-Asia-Running-file-January-to-December-2015-V2.csv"), na.strings="")
acledAf<-read.csv(paste0(pathData,"ACLED Version 6 All Africa 1997-2015_csv_dyadic.csv"), na.strings="")
#################

summ = function(x) {
  total=length(x)
  numNA = sum(is.na(x))
  leftovers = length(x) - numNA # also could this just be sum(!is.na(x))
  #result = c(total=total, numNA = numNA, leftovers = leftovers)
  result = c(leftovers = leftovers)
  return(result)}

summaryAf = tapply(acledAf$ACTOR2, acledAf$COUNTRY, summ ) 
summaryAf[order(summaryAf, decreasing=TRUE)]
range(acledAf$YEAR)  
