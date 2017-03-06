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
load(paste0(pathResults, 'ameResults.rda'))
rm(list=c('fit', 'fitFullSpec'))
###############

###############
# construct dyadic design array from list data
y = melt(yList) ; yLag = melt(yList) ; yLag$L1 = char(num(yLag$L1)-1)
xd = melt(xDyadL)
xd = cbind(xd[xd$Var3=='govActor',],
	postBoko=xd$value[xd$Var3=='postBoko'])
names(xd)[4]='govActor'
xr = dcast(melt(xRowL), Var1 + L1 ~ Var2)
xc = dcast(melt(xColL), Var1 + L1 ~ Var2)
glmData = cbind(y,xd[,c('govActor','postBoko')])
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

gfitFullSpec = glm(value ~ govActor + postBoko +
	riotsAgainst.row + protestsAgainst.row + vioCivEvents.row +
	riotsAgainst.col + protestsAgainst.col + vioCivEvents.col, 
	data=glmData, family='binomial')

gfitFullSpecLagDV = glm(value ~ lagDV + govActor + postBoko +
	riotsAgainst.row + protestsAgainst.row + vioCivEvents.row +
	riotsAgainst.col + protestsAgainst.col + vioCivEvents.col, 
	data=glmData, family='binomial')	
###############

###############
# save
save(
	gfitLagDV, gfitFullSpec, gfitFullSpecLagDV,
	file=paste0(pathResults, 'glmResults.rda')
	)
###############