################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R') }
################

################
# load data
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
load(paste0(pathResults, 'ameResults.rda'))
###############

###############
# construct dyadic design array from list data
yrs = char(2000:2016) ; yList = yList[yrs]
xDyadL = designArrays$base$dyadCovar ; dyadVars = dimnames(xDyadL[[1]])[[3]]
xRowL = designArrays$base$senCovar ; senVars = paste0(dimnames(xRowL[[1]])[[2]],'.row')
xColL = designArrays$base$recCovar ; recVars = paste0(dimnames(xColL[[1]])[[2]],'.col')

# break out of lists/arrays
y = melt(yList) ; yLag = melt(yList); yLag$L1 = char(num(yLag$L1)-1)
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
glmData$lagDV[is.na(glmData$lagDV)] = 0
###############

###############
# run GLM
gfitLagDV = glm(value ~ lagDV, data=glmData, family='binomial')

modSpecFull = formula( paste0(paste0('value ~ '), 
	paste(c(dyadVars, senVars, recVars), collapse=' + ') ) )
modSpecFullLagDV = formula( paste0(paste0('value ~ lagDV + '), 
	paste(c(dyadVars, senVars, recVars), collapse=' + ') ) )

gfitFullSpec = glm(modSpecFull, data=glmData, family='binomial')
gfitFullSpecLagDV = glm(modSpecFullLagDV, data=glmData, family='binomial')	
###############

###############
# save
save(
	gfitLagDV, gfitFullSpec, gfitFullSpecLagDV,
	file=paste0(pathResults, 'glmResults.rda')
	)
###############