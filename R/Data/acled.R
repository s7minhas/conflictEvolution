#################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
#################

#load data
acled<-read.csv(paste0(pathData,"ACLED-Asia-Running-file-January-to-December-2015-V2.csv"), na.strings="")
#################

summ = function(x) {
  total=length(x)
  numNA = sum(is.na(x))
  leftovers = length(x) - numNA # also could this just be sum(!is.na(x))
  result = c(total=total, numNA = numNA, leftovers = leftovers)
  return(result)}
  summary = tapply(acled$ACTOR2, acled$COUNTRY, summ ) 
  