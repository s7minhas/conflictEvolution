################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')}
loadPkg('devtools') ; devtools::install_github('s7minhas/amen') ; library(amen)
################

################
# load data
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
load(paste0(pathResults, 'ameResults.rda'))
################

################
# function to run k-fold cross validation analysis using ame
ameOutSamp = function(
	yList=yList, 
	xDyadL=NULL, xRowL=NULL, xColL=NULL,
	seed=6886, 
	R=2, model='bin', intercept=TRUE, rvar=TRUE, cvar=TRUE, symmetric=FALSE,
	burn=10000, nscan=20000, odens=25, folds=30, cores=6
	){
	
	################
	# divide dataset into folds randomly
	set.seed(seed)
	yListFolds = lapply(yList, function(y){
		yFold=matrix(sample(1:folds, length(y), replace=TRUE),
			nrow=nrow(y),ncol=ncol(y), dimnames=dimnames(y))
		diag(yFold) = NA
		return(yFold) })
	################
	
	################
	# run models by fold
	yCrossValTrain = lapply(1:folds, function(f){
		yListMiss = lapply(1:length(yList), function(t){
			foldID = yListFolds[[t]] ; y = yList[[t]]
			foldID[foldID==f]=NA ; y=y*foldID
			return(y) })
		names(yListMiss) = names(yList)
		return(yListMiss) }) ; names(yCrossValTrain) = char(1:folds)
	
	# run ame by fold
	loadPkg(c('doParallel', 'foreach'))
	cl=makeCluster(cores) ; registerDoParallel(cl)
	fitCrossVal <- foreach(ii=1:length(yCrossValTrain), 
		.packages=c('amen')) %dopar%{
		fit=ame_repL(
			Y=yCrossValTrain[[ii]], Xdyad=xDyadL, Xrow=xRowL, Xcol=xColL,
			symmetric=symmetric, rvar=rvar, cvar=cvar, R=R, 
			model=model, intercept=intercept, seed=seed,
			burn=burn, nscan=nscan, odens=odens, 
			plot=FALSE, gof=TRUE, periodicSave=FALSE )
			return(fit) }
	stopCluster(cl) ; names(fitCrossVal) = char(1:folds)

	# get preds
	outPerf = do.call('rbind', lapply(1:folds, function(f){
		fitFoldPred = fitCrossVal[[f]]$'EZ'
		do.call('rbind', lapply(1:length(fitFoldPred), function(t){
			predT = fitFoldPred[[t]]
			foldID = yListFolds[[t]] ; y = yList[[t]]
			covarMissInfo = design_array_listwisedel(xRowL[[t]], xColL[[t]], xDyadL[[t]], intercept, nrow(y))
			covarMissInfo = apply(covarMissInfo, c(1,2), sum)
			covarMissInfo[!is.na(covarMissInfo)] = 1
			foldID[foldID!=f]=NA ; foldID[!is.na(foldID)] = 1
			y=y*foldID*covarMissInfo ; predT=predT*foldID
			res=na.omit(data.frame(actual=c(y), pred=c(predT), fold=f, stringsAsFactors=FALSE))
			res$pred = 1/(1+exp(-res$pred))
			return(res) }) ) }) )
	
	# get binperfhelpers
	loadPkg(c('ROCR', 'RColorBrewer', 'caTools'))
	source(paste0(fPth, 'binPerfHelpers.R'))
	
	# get perf stats
	aucByFold=do.call('rbind', lapply(1:folds, function(f){
		slice = outPerf[outPerf$fold==f,]
		if(length(unique(slice$actual))==1){ return(NULL) }
		perf=cbind(fold=f,
			aucROC=getAUC(slice$pred, slice$actual),
			aucPR=auc_pr(slice$actual, slice$pred)
			)
		return(perf) } ))
	aucROC=getAUC(outPerf$pred, outPerf$actual)
	aucPR=auc_pr(outPerf$actual, outPerf$pred)
	################	
	
	# org output and return
	out=list(
		yCrossValTrain=yCrossValTrain,
		fitCrossVal=fitCrossVal,
		outPerf=outPerf, aucByFold=aucByFold,
		aucROC=aucROC, aucPR=aucPR
	)	
	return(out)
}
################

################
# load data
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
load(paste0(pathData, 'exoVars.rda')) # load xNodeL, xDyadL

# focus on post 2000 data [few actors beforehand]
yrs = char(2000:2016)
yList = yList[yrs] ; xDyadL = xDyadL[yrs] ; xNodeL = xNodeL[yrs]
###############

################
# set up model specs
subListArray = function(lA, vars, dims=2){
	if(dims==2){ return( lapply(lA, function(x){ x[,vars, drop=FALSE] }) ) }
	if(dims==3){ return( lapply(lA, function(x){ x[,,vars,drop=FALSE] }) ) } }
designArrays = list(
	NULL=list(senCovar=NULL, recCovar=NULL, dyadCovar=NULL), 
	base=list(
		senCovar=subListArray(xNodeL, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2),
		recCovar=subListArray(xNodeL, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2),
		dyadCovar=subListArray(xDyadL, c('govActor', 'postBoko', 'elecYear', 'ngbrConfCount'), 3)
		),
	noCiv=list(
		senCovar=subListArray(xNodeL, c('groupSpread'), 2),
		recCovar=subListArray(xNodeL, c('groupSpread'), 2),
		dyadCovar=subListArray(xDyadL, c('govActor', 'postBoko', 'elecYear', 'ngbrConfCount'), 3)
		),
	noBH=list(
		senCovar=subListArray(xNodeL, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2),
		recCovar=subListArray(xNodeL, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2),
		dyadCovar=subListArray(xDyadL, c('govActor', 'elecYear', 'ngbrConfCount'), 3)
		)	
	)

ameOutSamp_wFullSpec = ameOutSamp(
	yList=yList, 
	xDyadL=designArrays$base$dyadCovar,
	xRowL=designArrays$base$senCovar,
	xColL=designArrays$base$recCovar
	)

ameOutSamp_wNoCiv = ameOutSamp(
	yList=yList, 
	xDyadL=designArrays$noCiv$dyadCovar,
	xRowL=designArrays$noCiv$senCovar,
	xColL=designArrays$noCiv$recCovar
	)

ameOutSamp_wNoBH = ameOutSamp(
	yList=yList, 
	xDyadL=designArrays$noBH$dyadCovar,
	xRowL=designArrays$noBH$senCovar,
	xColL=designArrays$noBH$recCovar
	)

# save
save(
	ameOutSamp_wFullSpec, 
	ameOutSamp_wNoCiv,
	ameOutSamp_wNoBH,
	file=paste0(pathResults, 'ameCrossValResults.rda')
	)
################