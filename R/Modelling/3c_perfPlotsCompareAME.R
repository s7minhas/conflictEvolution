############################
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
source(paste0(fPth, 'binPerfHelpers.R'))
############################

############################
# org results
# ameOutSamp_wFullSpec, ameOutSamp_wNoCiv, ameOutSamp_wNoBH
load(paste0(pathResults, 'ameCrossValResults.rda'))
predDfs = list(
	cbind(ameOutSamp_wFullSpec$outPerf,model='AME (Base)'),
	cbind(ameOutSamp_wNoBH$outPerf,model='AME (No Boko Haram)'),
	cbind(ameOutSamp_wNoCiv$outPerf,model='AME (No Civ. Vars.)')
	)
names(predDfs) = c('AME (Base)', 'AME (No Boko Haram)', 'AME (No Civ. Vars.)')

# tabular data
aucSumm=do.call('rbind', lapply(predDfs,function(x){
	aucROC=getAUC(x$pred,x$actual) ; aucPR=auc_pr(x$actual,x$pred)
	return( c('AUC'=aucROC,'AUC (PR)'=aucPR) ) }) )
aucSumm = aucSumm[order(aucSumm[,1],decreasing=TRUE),]
aucSumm = trim(format(round(aucSumm, 2), nsmall=2))

<<<<<<< HEAD
# # roc data
# rocData=do.call('rbind', 
# 	lapply(predDfs, function(x){
# 		y=roc(x$pred,x$actual);y$model=unique(x$model);return(y) }))

# # precision recall curve data
# rocPrData=do.call('rbind', 
# 	lapply(predDfs, function(x){
# 		y=rocdf(x$pred,x$actual,type='pr');y$model=unique(x$model);return(y) }))
# ############################

# ############################
# # plotting

# # model col/lty
# ggCols = brewer.pal(length(levels(rocData$model)), 'Set1')[c(1,3,2)]
# ggLty = c('dashed', 'dotdash', 'solid')

# # Separation plots
# loadPkg(c('png','grid'))
# sepPngList = lapply(1:length(predDfs), function(ii){
# 	fSepPath = paste0(pathGraphics,'sep_',names(predDfs)[ii],'_outSample.png')
# 	# save as pngs for potential use outside of roc
# 	tmp = data.frame(act=predDfs[[ii]]$actual, proba=predDfs[[ii]]$'pred')
# 	ggSep(actual=tmp$act, proba=tmp$proba, 
# 		color=ggCols[ii], lty=ggLty[ii], fPath=fSepPath, save=TRUE )
# 	sepG = rasterGrob(readPNG(fSepPath), interpolate=TRUE)
# 	return(sepG)
# })

# # get rid of null model
# rocData$model = factor(rocData$model, levels=levels(rocData$model))
# rocPrData$model = factor(rocPrData$model, levels=levels(rocPrData$model))
# rownames(aucSumm)[grep('Lag DV + Covars', rownames(aucSumm), fixed=TRUE)]="GLM (Lag DV\n  + Covars)"

# tmp = rocPlot(rocData, linetypes=ggLty, colorManual=ggCols)+guides(linetype = FALSE, color = FALSE) ; yLo = -.04 ; yHi = .14
# for(ii in 1:length(sepPngList)){
# 	tmp = tmp + annotation_custom(sepPngList[[ii]], xmin=.5, xmax=1.05, ymin=yLo, ymax=yHi)
# 	yLo = yLo + .1 ; yHi = yHi + .1 }
# tmp = tmp + annotate('text', hjust=0, x=.51, y=seq(0.05,0.25,.1), label=names(predDfs), family="Source Sans Pro Light")
# ggsave(tmp, file=paste0(pathGraphics, 'roc_outSample.pdf'), width=5, height=5, device=cairo_pdf)

# tmp=rocPlot(rocPrData, type='pr', legText=12, legPos=c(.25,.35), legSpace=2, linetypes=ggLty, colorManual=ggCols) +
# 	guides(linetype=FALSE, color=FALSE) + 
# 	# geom_rect(xmin=-.05, ymin=.01, xmax=.45, ymax=.55, color='white', fill='white', size=.5) + 
# 	annotate('text', hjust=0, x=c(.4, .69, .88), y=1, 
# 		label=c('  ', ' AUC\n(ROC)', 'AUC\n(PR)'), family='Source Sans Pro Black', size=4) + 
# 	annotate('text', hjust=0, x=.4, y=seq(.63,.9,.13), 
# 		label=rev(rownames(aucSumm)), family='Source Sans Pro Light') + 
# 	annotate('text', hjust=0, x=.7, y=seq(.63,.9,.13), 
# 		label=rev(apply(aucSumm, 1, function(x){paste(x, collapse='     ')})),
# 		family='Source Sans Pro Light')
# ggsave(tmp, file=paste0(pathGraphics, 'rocPr_outSample.pdf'), width=5, height=5, device=cairo_pdf)
# ################################################
=======
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
ggLty = c('dashed', 'dotdash', 'solid')

# Separation plots
loadPkg(c('png','grid'))
sepPngList = lapply(1:length(predDfs), function(ii){
	fSepPath = paste0(pathGraphics,'sep_',names(predDfs)[ii],'_outSample.png')
	# save as pngs for potential use outside of roc
	tmp = data.frame(act=predDfs[[ii]]$actual, proba=predDfs[[ii]]$'pred')
	ggSep(actual=tmp$act, proba=tmp$proba, 
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
tmp = tmp + annotate('text', hjust=0, x=.51, y=seq(0.05,0.25,.1), label=names(predDfs), family="Source Sans Pro Light")
ggsave(tmp, file=paste0(pathGraphics, 'roc_outSample.pdf'), width=5, height=5, device=cairo_pdf)

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
ggsave(tmp, file=paste0(pathGraphics, 'rocPr_outSample.pdf'), width=5, height=5, device=cairo_pdf)
################################################
>>>>>>> refs/remotes/origin/master
