################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
loadPkg('devtools') ; devtools::install_github('s7minhas/amen') ; library(amen)
# get binperfhelpers
loadPkg(c('ROCR', 'RColorBrewer', 'caTools'))
source(paste0(fPth, 'binPerfHelpers.R'))
################

################
# load data
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
load(paste0(pathResults, 'ameResults.rda'))

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
pdsToForecast = c(1,5)
loadPkg(c('parallel','foreach'))
cores=length(pdsToForecast) ; cl=makeCluster(cores) ; registerDoParallel(cl)
ame_glm_outSampTime = foreach(dropFromEnd = pdsToForecast, 
	.packages=c('amen','ROCR','RColorBrewer','caTools')) %dopar% {

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
	# startVals = ameFits$base$startVals
	# tocut = (((dim( startVals$Z )[3]-(dropFromEnd))+1):dim( startVals$Z )[3])
	# startVals$Z = startVals$Z[,,-tocut ]
	fitIn_ame = ame_repL(
		Y=y_In, Xdyad=xDyadL_In, Xrow=xRowL_In, Xcol=xColL_In,
		symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
		model='bin', intercept=TRUE, seed=6886,
		burn=100000, nscan=500000, odens=25,
		plot=FALSE, gof=TRUE, periodicSave=FALSE
		# ,startVals=startVals
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
	yHat_Out = 1/(1+exp(-ez_Out))
	outPerf = data.frame(cbind(reshape2::melt(y_Out), pred=reshape2::melt(yHat_Out)[,3]))
	outPerf = outPerf[outPerf$Var1 != outPerf$Var2,]
	aucROC=getAUC(outPerf$pred, outPerf$value)
	aucPR=auc_pr(outPerf$value, outPerf$pred)
	ameBase = list(outPerf=outPerf$pred, aucROC=aucROC, aucPR=aucPR)

	return(list(glmFull=glmSpecFull, glmFullLagDV=glmSpecFullLagDV, ame=ameBase))
}
names(ame_glm_outSampTime) = paste0('last ', pdsToForecast, ' pd excluded')
################

################
# save
save(
	ame_glm_outSampTime, 
	file=paste0(pathResults, 'ameForecastResults.rda')
	)
################