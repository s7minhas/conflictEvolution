################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
loadPkg('devtools') ; devtools::install_github('s7minhas/amen') ; library(amen)
source(paste0(fPth, 'ameOutSamp.R'))
################

################
# load data
load(paste0(pathResults, 'ameResults.rda'))
################

################
# run outsamp models
ameOutSamp_NULL = ameOutSamp(
	yList=yList, xDyadL=NULL, xRowL=NULL, xColL=NULL, 
	startVals=fit$startVals
	)

ameOutSamp_wDyadCovar = ameOutSamp(
	yList=yList, xDyadL=xDyadL, xRowL=NULL, xColL=NULL, 
	startVals=fitDyadCovar$startVals
	)

ameOutSamp_wFullSpec = ameOutSamp(
	Y=yList, Xdyad=xDyadL, Xrow=xRowL, Xcol=xColL,
	startVals=fitFullSpec$startVals
	)

# save
save(
	ameOutSamp_NULL, ameOutSamp_wDyadCovar, ameOutSamp_wFullSpec, 
	file=paste0(pathResults, 'ameCrossValResults.rda')
	)
################