################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
source(paste0(fPth, 'actorInfo.R'))
################

################
# load data
load(paste0(pathResults, 'ameResults.rda')) # load AME mod results with yList used for modeling
# pull out array
yArr = listToArray(actors=getActor(yList), Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
################

################
# get total number of conf by T
unlist(lapply(yList, function(x){c(sum(x,na.rm=TRUE))}))
################
