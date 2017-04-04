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
load(paste0(pathResults, 'ameResults.rda'))

# glm data
y = melt(yList) ; yLag = melt(yList) ; yLag$L1 = char(num(yLag$L1)-1)
xd = melt(xDyadL)
xd = cbind(xd[xd$Var3=='govActor',],
	postBoko=xd$value[xd$Var3=='postBoko'],
	medianDist=xd$value[xd$Var3=='medianDist']
	)
names(xd)[4]='govActor'
xr = dcast(melt(xRowL), Var1 + L1 ~ Var2)
xc = dcast(melt(xColL), Var1 + L1 ~ Var2)
glmData = cbind(y,xd[,c('govActor','postBoko','medianDist')])
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
################

################
# cross val by time split

# organize data
dropFromEnd=1
y_In = yList[ 1:(length(yList)-dropFromEnd) ]
# xDyadL_In = xDyadL_noDist[ 1:(length(yList)-dropFromEnd) ]
xDyadL_In = xDyadL[ 1:(length(yList)-dropFromEnd) ]
xRowL_In = xRowL[ 1:(length(yList)-dropFromEnd) ]
xColL_In = xColL[ 1:(length(yList)-dropFromEnd) ]
glmData_In = glmData[which(glmData$L1 %in% names(yList)[1:(length(yList)-dropFromEnd)]), ]

y_Out = yList[[ length(yList) - (dropFromEnd - 1) ]]
# xDyadL_Out = xDyadL_noDist[[ length(yList) - (dropFromEnd - 1) ]]
xDyadL_Out = xDyadL[[ length(yList) - (dropFromEnd - 1) ]]
xRowL_Out = xRowL[[ length(yList) - (dropFromEnd - 1) ]]
xColL_Out = xColL[[ length(yList) - (dropFromEnd - 1) ]]
glmData_Out = glmData[which(glmData$L1 %in% names(yList)[length(yList) - (dropFromEnd - 1)]), ]

# # glm Formulas
# # run with lag dv
# glmOutSamp_wLagDV=glmOutSamp( glmForm=formula(value ~ lagDV) )

# # run with ame full spec
# glmOutSamp_wFullSpec=glmOutSamp(
# 	glmForm=formula(value ~ govActor + postBoko + medianDist + 
# 	rioProContra.row + vioCivEvents.row +
# 	rioProContra.col + vioCivEvents.col) )

# # ame full spec + lag DV
# glmOutSamp_wFullSpecLagDV=glmOutSamp(
# 	glmForm=formula(value ~ lagDV + govActor + postBoko + medianDist + 
# 	rioProContra.row + vioCivEvents.row +
# 	rioProContra.col + vioCivEvents.col) )

# run GLM
# glmForm=formula(value ~ lagDV + govActor + postBoko +
glmForm=formula(value ~ lagDV + govActor + postBoko + medianDist + 
	rioProContra.row + vioCivEvents.row +
	rioProContra.col + vioCivEvents.col) 
fitIn_glm = glm(glmForm, data=glmData_In, family='binomial')

# org glm results
glm_Pred = data.matrix(cbind(1, glmData_Out[,names(coef(fitIn_glm))[-1]])) %*% coef(fitIn_glm)
glm_Prob = 1/(1+exp(-glm_Pred))
outPerf_glm = data.frame(glmData_Out[,1:3], pred=glm_Prob)
aucROC_glm=getAUC(outPerf_glm$pred, outPerf_glm$value)
aucPR_glm=auc_pr(outPerf_glm$value, outPerf_glm$pred)

# run AME mods
# startVals = fitFullSpec_noDist$startVals
startVals = fitFullSpec$startVals
startVals$Z = startVals$Z[,,-dim( startVals$Z )[3] ]
fitIn_ame = ame_repL(
	Y=y_In, Xdyad=xDyadL_In, Xrow=xRowL_In, Xcol=xColL_In,
	symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
	model='bin', intercept=TRUE, seed=6886,
	burn=5, nscan=10, odens=1, 
	plot=FALSE, gof=TRUE, periodicSave=FALSE,
	startVals=startVals
	) 

# org AME results
beta = apply(fitIn_ame$BETA,2,mean)
a = fitIn_ame$APM[rownames(y_Out)]
b = fitIn_ame$BPM[rownames(y_Out)]
UVPM = fitIn_ame$UVPM[rownames(y_Out),rownames(y_Out)]

x_Out = array(1, 
	dim=c(  nrow(y_Out),nrow(y_Out), length(beta) ),
	dimnames=list( rownames(y_Out), rownames(y_Out), names(beta) ) )
x_Out[,,2:dim(x_Out)[3]] = design_array_listwisedel(xRowL_Out, xColL_Out, xDyadL_Out, TRUE, nrow(y_Out))

ez_Out = Xbeta(x_Out, beta) + outer(a,b,'+') + UVPM
yHat_Out = 1/(1+exp(-ez_Out))
outPerf = data.frame(cbind(reshape2::melt(y_Out), pred=reshape2::melt(yHat_Out)[,3]))
outPerf = outPerf[outPerf$Var1 != outPerf$Var2,]

aucROC=getAUC(outPerf$pred, outPerf$value)
aucPR=auc_pr(outPerf$value, outPerf$pred)

aucROC_glm
aucPR_glm

aucROC
aucPR
################