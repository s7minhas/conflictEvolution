################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R') }
source(paste0(fPth, 'binPerfHelpers.R'))
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
			fit = randomForest(rfForm, data=rfData, na.action=na.omit, ntree=500)
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
if(!file.exists(paste0(pathResults, 'rfCrossValResults.rda'))){
	rfOutSamp_wFullSpecLagDV=rfOutSamp( rfForm=modSpecFullLagDV, cores=4 )
	# save
	save(
		rfOutSamp_wFullSpecLagDV, 
		file=paste0(pathResults, 'rfCrossValResults.rda')
		) }
################

############################
# org results
load(paste0(pathResults, 'ameCrossValResults.rda')) # ameOutSamp_NULL, ameOutSamp_wFullSpec
load(paste0(pathResults, 'glmCrossValResults.rda')) # glmOutSamp_wFullSpec, glmOutSamp_wLagDV, glmOutSamp_wFullSpecLagDV
load(paste0(paste0(pathResults, 'rfCrossValResults.rda'))) # rfOutSamp_wFullSpecLagDV
predDfs = list(
	cbind(glmOutSamp_wFullSpec$outPerf,model='GLM (Covars)'), 
	cbind(glmOutSamp_wFullSpecLagDV$outPerf,model='GLM (Lag DV + Covars)'),
	cbind(rfOutSamp_wFullSpecLagDV$outPerf, model='RF (Lag DV + Covars)'),
	cbind(ameOutSamp_wFullSpec$outPerf,model='AME (Covars)')
	)
names(predDfs) = c(
	'GLM (Covars)', 'GLM (Lag DV + Covars)', 
	'RF (Lag DV + Covars)', 
	'AME (Covars)')

# tabular data
aucSumm=do.call('rbind', lapply(predDfs,function(x){
	aucROC=getAUC(x$pred,x$actual) ; aucPR=auc_pr(x$actual,x$pred)
	return( c('AUC'=aucROC,'AUC (PR)'=aucPR) ) }) )
aucSumm = aucSumm[order(aucSumm[,2],decreasing=TRUE),]
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
ggCols = brewer.pal(length(levels(rocData$model)), 'Set1')[c(1,3, 4, 2)]
ggLty = c('dashed', 'dotdash',  'twodash', 'solid')

# Separation plots
loadPkg(c('png','grid'))
sepPngList = lapply(1:length(predDfs), function(ii){
	fSepPath = paste0(pathGraphics,'sep_',names(predDfs)[ii],'_outSample.png')
	if(!file.exists(fSepPath)){
		# save as pngs for potential use outside of roc
		tmp = data.frame(act=predDfs[[ii]]$actual, proba=predDfs[[ii]]$'pred')
		ggSep(actual=tmp$act, proba=tmp$proba, 
			color=ggCols[ii], lty=ggLty[ii], fPath=fSepPath, save=TRUE )
	}
	sepG = rasterGrob(readPNG(fSepPath), interpolate=TRUE)
	return(sepG)
})

# get rid of null model
rocData$model = factor(rocData$model, levels=levels(rocData$model))
rocPrData$model = factor(rocPrData$model, levels=levels(rocPrData$model))
rownames(aucSumm)[2:3]=c(
	"RF (Lag DV\n  + Covars)",
	"GLM (Lag DV\n  + Covars)"
	)

tmp = rocPlot(rocData, linetypes=ggLty, colorManual=ggCols)+guides(linetype = FALSE, color = FALSE) ; yLo = -.04 ; yHi = .24
for(ii in 1:length(sepPngList)){
	tmp = tmp + annotation_custom(sepPngList[[ii]], xmin=.5, xmax=1.05, ymin=yLo, ymax=yHi)
	yLo = yLo + .1 ; yHi = yHi + .1 }
tmp = tmp + annotate('text', hjust=0, x=.51, y=seq(.1, .4, .1), label=names(predDfs), family="Source Sans Pro Light")
ggsave(tmp, 
	file=paste0(pathResults, 'revAppendix/roc_outSample_randForest.pdf'), 
	width=5, height=5, device=cairo_pdf)
ggsave(tmp, 
	file=paste0(pathGraphics, 'roc_outSample_randForest.pdf'), 
	width=5, height=5, device=cairo_pdf)

tmp=rocPlot(rocPrData, type='pr', legText=12, legPos=c(.25,.35), legSpace=2, linetypes=ggLty, colorManual=ggCols) +
	guides(linetype=FALSE, color=FALSE) + 
	# geom_rect(xmin=-.05, ymin=.01, xmax=.45, ymax=.55, color='white', fill='white', size=.5) + 
	annotate('text', hjust=0, x=c(.4, .69, .88), y=1, 
		label=c('  ', ' AUC\n(ROC)', 'AUC\n(PR)'), family='Source Sans Pro', size=4) + 
	annotate('text', hjust=0, x=.4, y=seq(.5,.9,.13), 
		label=rev(rownames(aucSumm)), family='Source Sans Pro Light') + 
	annotate('text', hjust=0, x=.7, y=seq(.5,.9,.13), 
		label=rev(apply(aucSumm, 1, function(x){paste(x, collapse='               ')})),
		family='Source Sans Pro Light')
ggsave(tmp, 
	file=paste0(pathResults, 'revAppendix/rocPR_outSample_randForest.pdf'),
	width=5, height=5, device=cairo_pdf)	
ggsave(tmp, 
	file=paste0(pathGraphics, 'rocPR_outSample_randForest.pdf'),
	width=5, height=5, device=cairo_pdf)	
################################################