############################
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
source(paste0(fPth, 'binPerfHelpers.R'))
############################

# loop through pds left out
for(dropFromEnd in c(1,5)){
	################################################
	# add forecast perf plots
	load(paste0(pathResults, 'ameForecastResults.rda')) # ame_glm_outSampTime
	aucSumm = melt( lapply(ame_glm_outSampTime, function(pdMod){
		lapply(pdMod, function(stats){ stats[2:3] }) }) )

	# org results
	predList = melt(lapply(ame_glm_outSampTime, function(pdMod){
		lapply(pdMod, function(stats){ stats[1] }) }))
	predDF = predList[
		predList$L1==paste0('last ',dropFromEnd,' pd excluded'),
		-which(names(predList) %in% c('Var1','Var2','L3','variable'))]

	# load in actual data
	load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
	yrs = char(2000:2016) ; yList = yList[yrs]
	y_Out = yList[[ length(yList) - (dropFromEnd - 1) ]]
	act = reshape2::melt(y_Out) ; act = act[act$Var1 != act$Var2,]
	predDF$act = rep(act$value, 3)

	# relabel model names
	modKey = data.frame(dirty=unique(predDF$L2))
	modKey$clean = c('GLM (Covars)', 'GLM (Lag DV + Covars)', 'AME (Covars)')
	predDF$model = modKey$clean[match(predDF$L2, modKey$dirty)]

	# ugh cleanup
	predDF$value[predDF$L2=='glmFull'] = (1/(1+exp(-predDF$value[predDF$L2=='glmFull'])))
	predDF$value[predDF$L2=='glmFullLagDV'] = (1/(1+exp(-predDF$value[predDF$L2=='glmFullLagDV'])))

	# tabular data
	aucSumm=aucSumm[
		aucSumm$L1==paste0('last ',dropFromEnd,' pd excluded'),
		-which(colnames(aucSumm) %in% c('L1'))
		]
	aucSumm = dcast(aucSumm, L2 ~ L3)[,c(3,2)]
	colnames(aucSumm) = c('AUC', 'AUC (PR)')
	rownames(aucSumm) = rev(modKey$clean)
	aucSumm = trim(format(round(data.matrix(aucSumm), 2), nsmall=2))
	aucSumm = aucSumm[order(aucSumm[,2],decreasing=TRUE),]
	modOrder = rev(rownames(aucSumm))

	# roc data
	rocData = do.call('rbind', 
		lapply(modKey$dirty, function(v){
			x = predDF[predDF$L2==v,]
			y=roc(x$value,x$act);y$model=unique(x$model);return(y) }) )
	rocData$model = factor(rocData$model, levels=modOrder)

	# precision recall curve data
	rocPrData = do.call('rbind', 
		lapply(modKey$dirty, function(v){
			x = predDF[predDF$L2==v,]
			y=rocdf(x$value,x$act,type='pr');y$model=unique(x$model);return(y) }) )
	rocPrData$model = factor(rocPrData$model, levels=modOrder)
	################################################

	################################################
	# plotting

	# model col/lty
	ggCols = brewer.pal(length(levels(rocData$model)), 'Set1')[c(3,1,2)]
	# ggLty = c('dashed', 'dotdash', 'solid')
	ggLty = c('dotdash', 'dashed', 'solid')

	# Separation plots
	loadPkg(c('png','grid'))
	sepPngList = lapply(1:nrow(aucSumm), function(ii){
		mName = modOrder[ii]
		fSepPath = paste0(pathGraphics,'sep_',mName,'_outSampleForecast_',dropFromEnd,'.png')
		# save as pngs for potential use outside of roc
		ggSep(actual=predDF$act[predDF$model==mName], proba=predDF$value[predDF$model==mName], 
			color=ggCols[ii], lty=ggLty[ii], fPath=fSepPath, save=TRUE )
		sepG = rasterGrob(readPNG(fSepPath), interpolate=TRUE)
		return(sepG)
	})

	# get rid of null model
	rocData$model = factor(rocData$model, levels=levels(rocData$model))
	rocPrData$model = factor(rocPrData$model, levels=levels(rocPrData$model))
	rownames(aucSumm)[grep('Lag DV + Covars', rownames(aucSumm), fixed=TRUE)]="GLM (Lag DV\n  + Covars)"

	tmp = rocPlot(rocData, linetypes=ggLty, colorManual=ggCols)+guides(linetype = FALSE, color = FALSE) ; yLo = -.04 ; yHi = .14
	for(ii in 1:length(sepPngList)){
		tmp = tmp + annotation_custom(sepPngList[[ii]], xmin=.5, xmax=1.05, ymin=yLo, ymax=yHi)
		yLo = yLo + .1 ; yHi = yHi + .1 }
	tmp = tmp + annotate('text', hjust=0, x=.51, y=seq(0.05,0.25,.1), label=modOrder, family="Source Sans Pro Light")
	ggsave(tmp, file=paste0(pathGraphics, 'roc_outSampleForecast_',dropFromEnd,'.pdf'), width=5, height=5, device=cairo_pdf)

	tmp=rocPlot(rocPrData, type='pr', legText=12, legPos=c(.25,.35), legSpace=2, linetypes=ggLty, colorManual=ggCols) +
		guides(linetype=FALSE, color=FALSE) + 
		# geom_rect(xmin=-.05, ymin=.01, xmax=.45, ymax=.55, color='white', fill='white', size=.5) + 
		annotate('text', hjust=0, x=c(.4, .69, .88), y=1, 
			label=c('  ', ' AUC\n(ROC)', 'AUC\n(PR)'), family='Source Sans Pro Black', size=4) + 
		annotate('text', hjust=0, x=.4, y=seq(.63,.9,.13), 
			label=rev(rownames(aucSumm)), family='Source Sans Pro Light') + 
		annotate('text', hjust=0, x=.7, y=seq(.63,.9,.13), 
			label=rev(apply(aucSumm, 1, function(x){paste(x, collapse='     ')})),
			family='Source Sans Pro Light')
	ggsave(tmp, file=paste0(pathGraphics, 'rocPr_outSampleForecast_',dropFromEnd,'.pdf'), width=5, height=5, device=cairo_pdf)
	################################################
}