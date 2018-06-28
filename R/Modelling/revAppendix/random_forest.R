################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R') }
loadPkg(c('randomForest','foreach','doParallel'))
################

################
# load data
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
load(paste0(pathResults, 'ameResults.rda'))

# crossval params
seed=6886
folds=30

# data
yrs = char(2000:2016) ; yList = yList[yrs]
xDyadL = designArrays$base$dyadCovar ; dyadVars = dimnames(xDyadL[[1]])[[3]]
xRowL = designArrays$base$senCovar ; senVars = paste0(dimnames(xRowL[[1]])[[2]],'.row')
xColL = designArrays$base$recCovar ; recVars = paste0(dimnames(xColL[[1]])[[2]],'.col')
################

################
rfOutSamp = function(rfForm, cores=4){
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

	# melt into rf format
	yCrossValTrain = lapply(yCrossValTrain, function(y){
		y = melt(y) ; yLag = melt(yList); yLag$L1 = char(num(yLag$L1)-1)
		yLagRecip = melt(lapply(yList,function(y){t(y)})); yLagRecip$L1 = char(num(yLagRecip$L1)-1)
		xd = dcast(melt(xDyadL), Var1 + Var2 + L1 ~ Var3)
		xd = xd[order(xd$L1, xd$Var2, xd$Var1),]
		xr = dcast(melt(xRowL), Var1 + L1 ~ Var2)
		xc = dcast(melt(xColL), Var1 + L1 ~ Var2)
		rfData = cbind(y,xd)
		for(v in names(xc)[3:ncol(xc)] ){
			rfData$tmp = xr[,v][match(paste0(rfData$Var1,rfData$L1),
				paste0(xr$Var1,xr$L1))]
			names(rfData)[ncol(rfData)] = paste0(v, '.row')
			rfData$tmp = xc[,v][match(paste0(rfData$Var2,rfData$L1),
				paste0(xc$Var1,xc$L1))]
			names(rfData)[ncol(rfData)] = paste0(v, '.col') }
		rfData = rfData[which(rfData$Var1!=rfData$Var2),]

		rfData$lagDV = yLag$value[match(
			paste0(rfData$Var1,rfData$Var2,rfData$L1), 
			paste0(yLag$Var1,yLag$Var2,yLag$L1))]
		rfData$lagDV[is.na(rfData$lagDV)] = 0

		rfData$lagRecip = yLagRecip$value[match(
			paste0(rfData$Var1,rfData$Var2,rfData$L1), 
			paste0(yLagRecip$Var1,yLagRecip$Var2,yLagRecip$L1))]
		rfData$lagRecip[is.na(rfData$lagRecip)] = 0		

		return(rfData)	
	})

	# run rf
	loadPkg(c('doParallel', 'foreach'))
	cl=makeCluster(cores) ; registerDoParallel(cl)
	fitCrossVal <- foreach(ii=1:length(yCrossValTrain), 
		.packages=c('randomForest')) %dopar%{
			rfData = yCrossValTrain[[ii]]
			rfData$value[rfData$value>1] = 1
			rfData = na.omit(rfData)
			rfData$value = factor(rfData$value)
			# default 500 trees
			fit = randomForest(rfForm, data=rfData, na.action=na.omit)
			return(fit) }
	stopCluster(cl)

	# get preds
	outPerf = do.call('rbind', lapply(1:folds, function(f){
		vars = rownames(fitCrossVal[[f]]$importance)
		# get probs
		testData = yCrossValTrain[[f]][is.na(yCrossValTrain[[f]]$value),vars]
		prob = predict(fitCrossVal[[f]], newdata=testData, type='prob')[,'1']

		# get actual
		actual=unlist(lapply(1:length(yListFolds), function(t){
				foldID = yListFolds[[t]] ; y = yList[[t]]
				foldID[foldID!=f]=NA ; foldID[!is.na(foldID)] = 1
				y=c(y*foldID) ; return(y[!is.na(y)])
			}))
		if(length(actual)!=length(prob)){stop('shit went wrong.')}
		res = data.frame(actual=actual, pred=prob, fold=f, stringsAsFactors = FALSE)
		if(any(grepl('lagDV',rfForm))){res=na.omit(res)}
		return(res)
	}))
		
	# get binperfhelpers
	loadPkg(c('ROCR', 'RColorBrewer', 'caTools'))
	source(paste0(fPth, 'binPerfHelpers.R'))

	# get perf stats
	aucByFold=do.call('rbind', lapply(1:folds, function(f){
		slice = outPerf[outPerf$fold==f,]
		slice = na.omit(slice)
		if(length(unique(slice$actual))==1){ return(NULL) }
		perf=cbind(fold=f,
			aucROC=getAUC(slice$pred, slice$actual),
			aucPR=auc_pr(slice$actual, slice$pred)
			)
		return(perf) } ))
	aucROC=getAUC(outPerf$pred, outPerf$actual)
	aucPR=auc_pr(outPerf$actual, outPerf$pred)
	################

	################
	out = list( yCrossValTrain=yCrossValTrain,
		fitCrossVal=fitCrossVal,
		outPerf=outPerf, aucByFold=aucByFold,
		aucROC=aucROC, aucPR=aucPR )
	return(out)
	################
}
################

################
# run with full specification and dv lag

# mod specs
modSpecFullLagDV = formula( paste0(paste0('value ~ lagDV + lagRecip + '), 
	paste(c(dyadVars, senVars, recVars), collapse=' + ') ) )

# ame full spec + lag DV
rfOutSamp_wFullSpecLagDV=rfOutSamp( rfForm=modSpecFullLagDV, cores=4 )

# save
save(
	rfOutSamp_wFullSpecLagDV, 
	file=paste0(pathResults, 'rfCrossValResults.rda')
	)
################