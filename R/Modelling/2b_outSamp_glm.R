################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R') }
loadPkg('devtools') ; devtools::install_github('s7minhas/amen') ; library(amen)
################

################
# load data
load(paste0(pathResults, 'ameResults.rda'))
rm(list=c('fit', 'fitDyadCovar', 'fitFullSpec'))

# crossval params
seed=6886
folds=30
################

################
glmOutSamp = function(glmForm){
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

	# melt into glm format
	yCrossValTrain = lapply(yCrossValTrain, function(y){
		y = melt(y)
		yLag = melt(yList) ; yLag$L1 = char(num(yLag$L1)-1)
		xd = melt(xDyadL) ; xd = cbind(xd[xd$Var3=='govActor',], postBoko=xd$value[xd$Var3=='postBoko'])
		names(xd)[4]='govActor'
		xr = melt(xRowL) ; xr = cbind(xr[xr$Var2=='riotsAgainst',], vioCivEvents=xr$value[xr$Var2=='vioCivEvents']) 
		names(xr)[3]='riotsAgainst'
		xc = melt(xColL) ; xc = cbind(xc[xc$Var2=='riotsAgainst',], vioCivEvents=xc$value[xc$Var2=='vioCivEvents']) 
		names(xc)[3]='riotsAgainst'
		glmData = cbind(y,xd[,c('govActor','postBoko')])
		glmData$riotsAgainst.row = xr$riotsAgainst[match(paste0(glmData$Var1,glmData$L1),paste0(xr$Var1,xr$L1))]
		glmData$riotsAgainst.col = xc$riotsAgainst[match(paste0(glmData$Var2,glmData$L1),paste0(xc$Var1,xc$L1))]
		glmData$vioCivEvents.row = xr$vioCivEvents[match(paste0(glmData$Var1,glmData$L1),paste0(xr$Var1,xr$L1))]
		glmData$vioCivEvents.col = xc$vioCivEvents[match(paste0(glmData$Var2,glmData$L1),paste0(xc$Var1,xc$L1))]
		glmData = glmData[which(glmData$Var1!=glmData$Var2),]
		glmData$lagDV = yLag$value[match(paste0(glmData$Var1,glmData$Var2,glmData$L1), paste0(yLag$Var1,yLag$Var2,yLag$L1))]
		return(glmData)	
	})

	# run glm
	fitCrossVal = lapply(yCrossValTrain, function(glmData){
		glmData$value[glmData$value>1] = 1
		fit = glm(glmForm, data=glmData, family='binomial')	
		return(fit)
	})

	# get preds
	outPerf = do.call('rbind', lapply(1:folds, function(f){
		# get probs
		testData = cbind(int=1,yCrossValTrain[[f]][is.na(yCrossValTrain[[f]]$value),names(coef(fitCrossVal[[f]]))[-1]])
		prob = 1/(1+exp(-as.matrix(testData) %*% coef(fitCrossVal[[f]])))

		# get actual
		actual=unlist(lapply(1:length(yListFolds), function(t){
				foldID = yListFolds[[t]] ; y = yList[[t]]
				foldID[foldID!=f]=NA ; foldID[!is.na(foldID)] = 1
				y=c(y*foldID) ; return(y[!is.na(y)])
			}))
		if(length(actual)!=length(prob)){stop('shit went wrong.')}
		res = data.frame(actual=actual, pred=prob, fold=f, stringsAsFactors = FALSE)
		if(any(grepl('lagDV',glmForm))){res=na.omit(res)}
		return(res)
	}))
		
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
# run with lag dv
glmOutSamp_wLagDV=glmOutSamp( glmForm=formula(value ~ lagDV) )

# run with ame full spec
glmOutSamp_wFullSpec=glmOutSamp(
	glmForm=formula(value ~ 
		govActor + postBoko + 
		riotsAgainst.row + vioCivEvents.row + 
		riotsAgainst.col + vioCivEvents.col) )

# ame full spec + lag DV
glmOutSamp_wFullSpecLagDV=glmOutSamp(
	glmForm=formula(value ~ lagDV + 
		govActor + postBoko + 
		riotsAgainst.row + vioCivEvents.row + 
		riotsAgainst.col + vioCivEvents.col) )

# save
save(
	glmOutSamp_wFullSpec, glmOutSamp_wLagDV,
	glmOutSamp_wFullSpecLagDV,
	file=paste0(pathResults, 'glmCrossValResults.rda')
	)
################