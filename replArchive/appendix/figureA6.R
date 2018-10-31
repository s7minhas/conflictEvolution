################
# workspace
source('../main/setup.R')
library(amen)
loadPkg(c('ROCR', 'RColorBrewer', 'caTools'))
source('../main/binPerfHelpers.R')
################

################
# load data
load('../main/nigeriaMatList_acled_v7.rda') # loads yList object
load('../main/ameResults.rda')

# construct dyadic design array from list data
yrs = char(2000:2016) ; yList = yList[yrs]
xDyadL = designArrays$base$dyadCovar ; dyadVars = dimnames(xDyadL[[1]])[[3]]
xRowL = designArrays$base$senCovar ; senVars = paste0(dimnames(xRowL[[1]])[[2]],'.row')
xColL = designArrays$base$recCovar ; recVars = paste0(dimnames(xColL[[1]])[[2]],'.col')

# break out of lists/arrays
y = melt(yList) ; yLag = melt(yList); yLag$L1 = char(num(yLag$L1)-1)
yLagRecip = melt(lapply(yList,function(y){t(y)})); yLagRecip$L1 = char(num(yLagRecip$L1)-1)
xd = dcast(melt(xDyadL), Var1 + Var2 + L1 ~ Var3)
xd = xd[order(xd$L1, xd$Var2, xd$Var1),]
xr = dcast(melt(xRowL), Var1 + L1 ~ Var2)
xc = dcast(melt(xColL), Var1 + L1 ~ Var2)

# combine into df
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
glmData$lagRecip = yLagRecip$value[match(
	paste0(glmData$Var1,glmData$Var2,glmData$L1), 
	paste0(yLagRecip$Var1,yLagRecip$Var2,yLagRecip$L1))]
glmData$lagRecip[is.na(glmData$lagRecip)] = 0		
glmData$lagDV[is.na(glmData$lagDV)] = 0
################

################
# get glm info
glmPredInfo = function(fitIn_glm, glmData_Out){
	glm_Pred = data.matrix(cbind(1, glmData_Out[,names(coef(fitIn_glm))[-1]])) %*% coef(fitIn_glm)
	glm_Prob = 1/(1+exp(-glm_Pred))
	outPerf_glm = data.frame(glmData_Out[,1:3], pred=glm_Prob)
	aucROC_glm=getAUC(outPerf_glm$pred, outPerf_glm$value)
	aucPR_glm=auc_pr(outPerf_glm$value, outPerf_glm$pred)
	return(list(outPerf=glm_Pred, aucROC=aucROC_glm, aucPR=aucPR_glm)) }
################

################
# cross val by time split
pdsToForecast = 1
ame_glm_outSampTime = lapply(pdsToForecast, function(dropFromEnd){
	# 
	modLagDV = formula('value ~ lagDV + lagRecip')
	modSpecFull = formula( paste0(paste0('value ~ '), 
		paste(c(dyadVars, senVars, recVars), collapse=' + ') ) )
	modSpecFullLagDV = formula( paste0(paste0('value ~ lagDV + lagRecip + '), 
		paste(c(dyadVars, senVars, recVars), collapse=' + ') ) )

	# leave out some number of period
	y_In = yList[ 1:(length(yList)-dropFromEnd) ]
	xDyadL_In = xDyadL[ 1:(length(yList)-dropFromEnd) ]
	xRowL_In = xRowL[ 1:(length(yList)-dropFromEnd) ]
	xColL_In = xColL[ 1:(length(yList)-dropFromEnd) ]
	glmData_In = glmData[which(glmData$L1 %in% names(yList)[1:(length(yList)-dropFromEnd)]), ]

	y_Out = yList[[ length(yList) - (dropFromEnd - 1) ]]
	xDyadL_Out = xDyadL[[ length(yList) - (dropFromEnd - 1) ]]
	xRowL_Out = xRowL[[ length(yList) - (dropFromEnd - 1) ]]
	xColL_Out = xColL[[ length(yList) - (dropFromEnd - 1) ]]
	glmData_Out = glmData[which(glmData$L1 %in% names(yList)[length(yList) - (dropFromEnd - 1)]), ]

	# run GLM
	glmSpecFull = glmPredInfo(glm(modSpecFull, data=glmData_In, family='binomial'), glmData_Out)
	glmSpecFullLagDV = glmPredInfo(glm(modSpecFullLagDV, data=glmData_In, family='binomial'), glmData_Out)

	##########
	# run AME base mod
	fitIn_ame = ame_repL(
		Y=y_In, Xdyad=xDyadL_In, Xrow=xRowL_In, Xcol=xColL_In,
		symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
		model='bin', intercept=TRUE, seed=6886,
		burn=100000, nscan=500000, odens=25,
		plot=FALSE, gof=TRUE, periodicSave=FALSE
		) 

	# org AME results
	beta = apply(fitIn_ame$BETA,2,mean)
	a = fitIn_ame$APM[rownames(y_Out)]
	b = fitIn_ame$BPM[rownames(y_Out)]
	UVPM = fitIn_ame$UVPM[rownames(y_Out),rownames(y_Out)]

	# ame design array for out sample
	x_Out = array(1, 
		dim=c(  nrow(y_Out),nrow(y_Out), length(beta) ),
		dimnames=list( rownames(y_Out), rownames(y_Out), names(beta) ) )
	x_Out[,,2:dim(x_Out)[3]] = design_array_listwisedel(xRowL_Out, xColL_Out, xDyadL_Out, TRUE, nrow(y_Out))

	# run calcs
	ez_Out = Xbeta(x_Out, beta) + outer(a,b,'+') + UVPM
	yHat_Out = pnorm(ez_Out)
	outPerf = data.frame(cbind(reshape2::melt(y_Out), pred=reshape2::melt(yHat_Out)[,3]))
	outPerf = outPerf[outPerf$Var1 != outPerf$Var2,]
	aucROC=getAUC(outPerf$pred, outPerf$value)
	aucPR=auc_pr(outPerf$value, outPerf$pred)
	ameBase = list(outPerf=outPerf$pred, aucROC=aucROC, aucPR=aucPR)

	return(list(glmFull=glmSpecFull, glmFullLagDV=glmSpecFullLagDV, ame=ameBase))
})
names(ame_glm_outSampTime) = paste0('last ', pdsToForecast, ' pd excluded')
################

# loop through pds left out
dropFromEnd = 1
################################################
# add forecast perf plots
aucSumm = melt( lapply(ame_glm_outSampTime, function(pdMod){
	lapply(pdMod, function(stats){ stats[2:3] }) }) )

# org results
predList = melt(lapply(ame_glm_outSampTime, function(pdMod){
	lapply(pdMod, function(stats){ stats[1] }) }))
predDF = predList[
	predList$L1==paste0('last ',dropFromEnd,' pd excluded'),
	-which(names(predList) %in% c('Var1','Var2','L3','variable'))]

# load in actual data
load('../main/nigeriaMatList_acled_v7.rda') # loads yList object
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
ggLty = c('dotdash', 'dashed', 'solid')

# Separation plots
loadPkg(c('png','grid'))
sepPngList = lapply(1:nrow(aucSumm), function(ii){
	mName = modOrder[ii]
	fSepPath = paste0('sep_',mName,'_outSampleForecast_',dropFromEnd,'.png')
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

tmp = rocPlot(rocData, linetypes=ggLty, colorManual=ggCols) + 
	guides(linetype = FALSE, color = FALSE) ; yLo = -.04 ; yHi = .14
for(ii in 1:length(sepPngList)){
	tmp = tmp + annotation_custom(sepPngList[[ii]], xmin=.5, xmax=1.05, ymin=yLo, ymax=yHi)
	yLo = yLo + .1 ; yHi = yHi + .1 }
tmp = tmp + annotate('text', hjust=0, x=.51, y=seq(0.05,0.25,.1), label=modOrder)
ggsave(tmp, file='floats/figureA6_a.pdf', width=5, height=5)

tmp=rocPlot(rocPrData, type='pr', legText=12, 
	legPos=c(.25,.35), legSpace=2, linetypes=ggLty, colorManual=ggCols) +
	guides(linetype=FALSE, color=FALSE) + 
	annotate('text', hjust=0, x=c(.4, .69, .88), y=1, 
		label=c('  ', ' AUC\n(ROC)', 'AUC\n(PR)'), size=4) + 
	annotate('text', hjust=0, x=.4, y=seq(.63,.9,.13), 
		label=rev(rownames(aucSumm))) + 
	annotate('text', hjust=0, x=.7, y=seq(.63,.9,.13), 
		label=rev(apply(aucSumm, 1, function(x){paste(x, collapse='     ')})) )
ggsave(tmp, file='floats/figureA6_b.pdf', width=5, height=5)
################################################