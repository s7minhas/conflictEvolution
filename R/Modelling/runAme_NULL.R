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
# Boko Haram enters in 2009
yListPreBH = yList[char(1998:2008)]
yListPostBH = yList[char(2009:2014)]
################

################
loadPkg('devtools') ; devtools::install_github('s7minhas/amen') ; library(amen)

fit=ame_repL(
	Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL, 
	symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
	model='bin', intercept=TRUE, seed=6886,
	burn=50000, nscan=25000, odens=25, 
	plot=FALSE, gof=TRUE, periodicSave=FALSE
	)

fitPreBH=ame_repL(
	Y=yListPreBH, Xdyad=NULL, Xrow=NULL, Xcol=NULL, 
	symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
	model='bin', intercept=TRUE, seed=6886,
	burn=50000, nscan=25000, odens=25, 
	plot=FALSE, gof=TRUE, periodicSave=FALSE
	)

fitPostBH=ame_repL(
	Y=yListPostBH, Xdyad=NULL, Xrow=NULL, Xcol=NULL, 
	symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
	model='bin', intercept=TRUE, seed=6886,
	burn=50000, nscan=25000, odens=25, 
	plot=FALSE, gof=TRUE, periodicSave=FALSE
	)
################

################
# run ame with covars
fitDyadCovar=ame_repL(
	Y=yList, Xdyad=xDyadL, Xrow=NULL, Xcol=NULL, 
	symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
	model='bin', intercept=TRUE, seed=6886,
	burn=25000, nscan=100000, odens=25, 
	plot=FALSE, gof=TRUE, periodicSave=FALSE
	)
################

################
# save
save(
	fit, fitPreBH, fitPostBH, 
	fitDyadCovar,
	yList, yListPreBH, yListPostBH, 
	xNodeL, xDyadL,
	file=paste0(pathResults, 'ameResults.rda')
	)
################