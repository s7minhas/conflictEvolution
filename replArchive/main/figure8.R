setwd('~/Research/conflictEvolution/replArchive/main/')
################
# workspace
source('setup.R')
library(amen)
loadPkg(
	c(
		'doParallel', 'foreach',
		'ROCR', 'RColorBrewer', 'caTools'
		)
	)
source('binPerfHelpers.R')
################

################
# load data
load('nigeriaMatList_acled_v7.rda') # loads yList object
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
			res$pred = pnorm(res$pred)
			return(res) }) ) }) )
	
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
load('nigeriaMatList_acled_v7.rda') # loads yList object
load('exoVars.rda') # load xNodeL, xDyadL

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
	base=list(
		senCovar=subListArray(xNodeL, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2),
		recCovar=subListArray(xNodeL, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2),
		dyadCovar=subListArray(xDyadL, c('govActor', 'postBoko', 'elecYear', 'ngbrConfCount'), 3)
		)
	)

# run outsamp fn
if(!file.exists('ameCrossValResults.rda')){
	ameOutSamp_wFullSpec = ameOutSamp(
		yList=yList, 
		xDyadL=designArrays$base$dyadCovar,
		xRowL=designArrays$base$senCovar,
		xColL=designArrays$base$recCovar
		)

	# save
	save(
		ameOutSamp_wFullSpec, 
		file='ameCrossValResults.rda'
		)
} else { load('ameCrossValResults.rda') }
################

################
## GLM cross val
# data
yrs = char(2000:2016) ; yList = yList[yrs]
xDyadL = designArrays$base$dyadCovar ; dyadVars = dimnames(xDyadL[[1]])[[3]]
xRowL = designArrays$base$senCovar ; senVars = paste0(dimnames(xRowL[[1]])[[2]],'.row')
xColL = designArrays$base$recCovar ; recVars = paste0(dimnames(xColL[[1]])[[2]],'.col')
################

################
glmOutSamp = function(glmForm, folds=30, seed=6886){
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
		yLagRecip = melt(lapply(yList,function(y){t(y)})); yLagRecip$L1 = char(num(yLagRecip$L1)-1)
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

		glmData$lagRecip = yLagRecip$value[match(
			paste0(glmData$Var1,glmData$Var2,glmData$L1), 
			paste0(yLagRecip$Var1,yLagRecip$Var2,yLagRecip$L1))]
		glmData$lagRecip[is.na(glmData$lagRecip)] = 0		

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
# mod specs
modSpecFull = formula( paste0(paste0('value ~ '), 
	paste(c(dyadVars, senVars, recVars), collapse=' + ') ) )
modSpecFullLagDV = formula( paste0(paste0('value ~ lagDV + lagRecip + '), 
	paste(c(dyadVars, senVars, recVars), collapse=' + ') ) )

if(!file.exists('glmCrossValResults.rda')){
	# run with ame full spec
	glmOutSamp_wFullSpec=glmOutSamp( glmForm=modSpecFull )

	# ame full spec + lag DV
	glmOutSamp_wFullSpecLagDV=glmOutSamp( glmForm=modSpecFullLagDV )

	# save
	save(
		glmOutSamp_wFullSpec, 
		glmOutSamp_wFullSpecLagDV, 
		file='glmCrossValResults.rda'
		)
} else {
	load('glmCrossValResults.rda')
}
################

################
## summarize model perf
predDfs = list(
	cbind(glmOutSamp_wFullSpec$outPerf,model='GLM (Covars)'), 
	cbind(glmOutSamp_wFullSpecLagDV$outPerf,model='GLM (Lag DV + Covars)'),
	cbind(ameOutSamp_wFullSpec$outPerf,model='AME (Covars)')
	)
names(predDfs) = c('GLM (Covars)', 'GLM (Lag DV + Covars)', 'AME (Covars)')

# tabular data
aucSumm=do.call('rbind', lapply(predDfs,function(x){
	aucROC=getAUC(x$pred,x$actual) ; aucPR=auc_pr(x$actual,x$pred)
	return( c('AUC'=aucROC,'AUC (PR)'=aucPR) ) }) )
aucSumm = aucSumm[order(aucSumm[,1],decreasing=TRUE),]
aucSumm = trim(format(round(aucSumm, 2), nsmall=2))

# roc data
rocData=do.call('rbind', 
	lapply(predDfs, function(x){
		y=roc(x$pred,x$actual);y$model=unique(x$model);return(y) }))

# precision recall curve data
rocPrData=do.call('rbind', 
	lapply(predDfs, function(x){
		y=rocdf(x$pred,x$actual,type='pr');y$model=unique(x$model);return(y) }))
############################

############################
# plotting

# model col/lty
ggCols = brewer.pal(length(levels(rocData$model)), 'Set1')[c(1,3,2)]
ggCols = brewer.pal(9, 'Greys')[c(4,6,8)]
ggLty = c('dashed', 'dotdash', 'solid')

# Separation plots
loadPkg(c('png','grid'))
sepPngList = lapply(1:length(predDfs), function(ii){
	fSepPath = paste0('sep_',names(predDfs)[ii],'_outSample_bw.png')
	# save as pngs for potential use outside of roc
	tmp = data.frame(act=predDfs[[ii]]$actual, proba=predDfs[[ii]]$'pred')
	ggSep(actual=tmp$act, proba=tmp$proba+.01, 
		color=ggCols[ii], lty=ggLty[ii], fPath=fSepPath, save=TRUE )
	sepG = rasterGrob(readPNG(fSepPath), interpolate=TRUE)
	return(sepG) })

# get rid of null model
rocData$model = factor(rocData$model, levels=levels(rocData$model))
rocPrData$model = factor(rocPrData$model, levels=levels(rocPrData$model))
rownames(aucSumm)[
	grep('Lag DV + Covars', 
		rownames(aucSumm), fixed=TRUE)
	]="GLM (Lag DV\n  + Covars)"

tmp = rocPlot(rocData, linetypes=ggLty, colorManual=ggCols) +
	guides(linetype = FALSE, color = FALSE) ; yLo = -.04 ; yHi = .14
for(ii in 1:length(sepPngList)){
	tmp = tmp + annotation_custom(sepPngList[[ii]], xmin=.5, xmax=1.05, ymin=yLo, ymax=yHi)
	yLo = yLo + .1 ; yHi = yHi + .1 }
tmp = tmp + annotate(
	'text', 
	hjust=0, x=.51, y=seq(0.05,0.25,.1), label=names(predDfs),
	family="Source Sans Pro Light")
ggsave(tmp, file='floats/figure8_a_bw.pdf', width=5, height=5,
	device=cairo_pdf)

tmp=rocPlot(rocPrData, type='pr', legText=12, legPos=c(.25,.35), 
	legSpace=2, linetypes=ggLty, colorManual=ggCols) +
	guides(linetype=FALSE, color=FALSE) + 
	annotate('text', hjust=0, x=c(.4, .69, .81), y=1, 
		label=c('  ', ' AUC\n(ROC)', 'AUC\n(PR)'), family='Source Sans Pro', size=3) + 
	annotate('text', hjust=0, x=.4, y=seq(.63,.9,.13), 
		label=rev(rownames(aucSumm)), family='Source Sans Pro Light') + 
	annotate('text', hjust=0, x=.7, y=seq(.63,.9,.13), 
		label=rev(apply(aucSumm, 1, function(x){paste(x, collapse='     ')})),
		family='Source Sans Pro Light')
ggsave(tmp, file='floats/figure8_b_bw.pdf', width=5, height=5,
	device=cairo_pdf)
################
system('open floats/figure8_a_bw.pdf')
system('open floats/figure8_b_bw.pdf')