################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
################

################
# load data
load(paste0(pathResults, 'ameResults.rda'))
rm(list=c('fit', 'fitDyadCovar', 'fitFullSpec'))
###############

###############
# construct dyadic design array from list data
y = melt(yList) ; yLag = melt(yList) ; yLag$L1 = char(num(yLag$L1)-1)
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
###############

###############
# run GLM
gfitLagDV = glm(value ~ lagDV, data=glmData, family='binomial')

gfitFullSpec = glm(value ~ govActor + postBoko + riotsAgainst.row + 
	vioCivEvents.row + riotsAgainst.col + vioCivEvents.col, 
	data=glmData, family='binomial')

gfitFullSpecLagDV = glm(value ~ lagDV + govActor + postBoko + riotsAgainst.row +
	vioCivEvents.row + riotsAgainst.col + vioCivEvents.col,
	data=glmData, family='binomial')	
###############

###############
# save
save(
	gfitLagDV, gfitFullSpec, gfitFullSpecLagDV,
	file=paste0(pathResults, 'glmResults.rda')
	)
###############