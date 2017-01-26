################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
################

################
# load data
load(paste0(pathData, 'nigeriaMatList.rda')) # loads yList object
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

# create a simple design array
actors=unique(unlist(lapply(yList, rownames)))
actorInfo = matrix(0, nrow=length(actors),ncol=1,dimnames=list(actors,c('gov')))
govActors=c('Military Forces of Nigeria','Police Forces of Nigeria')
xNodeL = lapply(1:length(yList), function(t){
	actors = rownames( yList[[t]] )
	yr = num(names(yList)[t])
	xMat = matrix(0,nrow=length(actors),ncol=2,dimnames=list(actors,c('govActor','postBoko')))
	xMat[which(rownames(xMat) %in% govActors),'govActor'] = 1
	if(yr>2008){xMat[,'postBoko']=1} # boko enters network in 2009
	return(xMat)
}) ; names(xNodeL) = names(yList)

xDyadL = lapply(1:length(yList), function(t){
	actors = rownames( yList[[t]] )
	yr = num(names(yList)[t])
	xArr = array(0,dim=c(length(actors),length(actors),2),dimnames=list(actors,actors,c('govActor','postBoko')))
	xArr[which(rownames(xArr) %in% govActors),which(colnames(xArr) %in% govActors),'govActor'] = 1
	if(yr>2008){xArr[,,'postBoko']=1} # boko enters network in 2009
	for(p in 1:dim(xArr)[3]){ diag(xArr[,,p])=NA }
	return(xArr)
}) ; names(xDyadL) = names(yList)

fitCovar=ame_repL(
	Y=yList, Xdyad=NULL, Xrow=xNodeL, Xcol=xNodeL, 
	symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
	model='bin', intercept=TRUE, seed=6886,
	burn=50000, nscan=25000, odens=25, 
	plot=FALSE, gof=TRUE, periodicSave=FALSE
	)

fitDyadCovar=ame_repL(
	Y=yList, Xdyad=xDyadL, Xrow=NULL, Xcol=NULL, 
	symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
	model='bin', intercept=TRUE, seed=6886,
	burn=50000, nscan=25000, odens=25, 
	plot=FALSE, gof=TRUE, periodicSave=FALSE,
	)
################

################
# save
save(
	fit, fitPreBH, fitPostBH, 
	fitCovar, fitDyadCovar,
	yList, yListPreBH, yListPostBH, 
	xNodeL, xDyadL,
	file=paste0(pathResults, 'ameResults.rda')
	)
################