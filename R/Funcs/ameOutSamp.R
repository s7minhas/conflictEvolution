ameOutSamp = function(
	yList, xDyadL=NULL, xRowL=NULL, xColL=NULL, startVals,
	seed=6886, folds=30, 
	R=2, model='bin', burn=10000, nscan=2000, odens=25, 
	intercept=TRUE, rvar=TRUE, cvar=TRUE, symmetric=FALSE
	){
	
	################
	# divide dataset into folds
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
	fitCrossVal = lapply(yCrossValTrain, function(yCV){
		fit=ame_repL(
			Y=yCV, Xdyad=xDyadL, Xrow=xRowL, Xcol=xColL,
			symmetric=symmetric, rvar=rvar, cvar=cvar, R=R, 
			model=model, intercept=intercept, seed=seed,
			burn=burn, nscan=nscan, odens=odens, 
			plot=FALSE, gof=TRUE, periodicSave=FALSE,
			startVals=startVals
			)
		return(fit) })
	
	# get preds
	outPerf = do.call('rbind', lapply(1:folds, function(f){
		fitFoldPred = fitCrossVal[[f]]$'EZ'
		do.call('rbind', lapply(1:length(fitFoldPred), function(t){
			predT = fitFoldPred[[t]]
			foldID = yListFolds[[t]] ; y = yList[[t]]
			foldID[foldID!=f]=NA ; foldID[!is.na(foldID)] = 1
			y=y*foldID ; predT=predT*foldID
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
