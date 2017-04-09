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
		y = melt(y) ; yLag = melt(yList); yLag$L1 = char(num(yLag$L1)-1)
		xd = dcast(melt(xDyadL), Var1 + Var2 + L1 ~ Var3)
		xd = xd[order(xd$L1, xd$Var2, xd$Var1),]
		xr = dcast(melt(xRowL), Var1 + L1 ~ Var2)
		xc = dcast(melt(xColL), Var1 + L1 ~ Var2)
		glmData = cbind(y,xd)
		for(v in names(xc)[3:ncol(xc)] ){
			glmData$tmp = xr[,v][match(paste0(glmData$Var1,glmData$L1),
				paste0(xr$Var1,xr$L1))]
			names(glmData)[ncol(glmData)] = paste0(v, '.row')
			glmData$tmp = xc[,v][match(paste0(glmData$Var2,glmData$L1),
				paste0(xc$Var1,xc$L1))]
			names(glmData)[ncol(glmData)] = paste0(v, '.col') }
		glmData = glmData[which(glmData$Var1!=glmData$Var2),]
		glmData$lagDV = yLag$value[match(
			paste0(glmData$Var1,glmData$Var2,glmData$L1), 
			paste0(yLag$Var1,yLag$Var2,yLag$L1))]
		glmData$lagDV[is.na(glmData$lagDV)] = 0
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
# run with lag dv
glmOutSamp_wLagDV=glmOutSamp( glmForm=formula(value ~ lagDV) )

# mod specs
modSpecFull = formula( paste0(paste0('value ~ '), 
	paste(c(dyadVars, senVars, recVars), collapse=' + ') ) )
modSpecFullLagDV = formula( paste0(paste0('value ~ lagDV + '), 
	paste(c(dyadVars, senVars, recVars), collapse=' + ') ) )

# run with ame full spec
glmOutSamp_wFullSpec=glmOutSamp( glmForm=modSpecFull )

# ame full spec + lag DV
glmOutSamp_wFullSpecLagDV=glmOutSamp( glmForm=modSpecFullLagDV )

# save
save(
	glmOutSamp_wFullSpec, glmOutSamp_wLagDV,
	glmOutSamp_wFullSpecLagDV,
	file=paste0(pathResults, 'glmCrossValResults.rda')
	)
################