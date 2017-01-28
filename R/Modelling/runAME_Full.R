################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
################

################
# load data
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
load(paste0(pathData, 'exoVars.rda')) # load xNodeL, xDyadL
###############

################
loadPkg('devtools') ; devtools::install_github('s7minhas/amen') ; library(amen)

xNodeL = lapply(xNodeL, function(x){ x[,c('vioCivEvents','riotsAgainst')] })
fit=ame_repL(
	Y=yList, Xdyad=xDyadL, Xrow=xNodeL, Xcol=xNodeL, 
	symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
	model='bin', intercept=TRUE, seed=6886,
	# burn=25000, nscan=100000, odens=25, 
	burn=10000, nscan=20000, odens=20, 	
	plot=FALSE, gof=TRUE, periodicSave=FALSE
	)
################