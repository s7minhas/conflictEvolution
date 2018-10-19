################
# workspace
source('../setup.R')
source('../paramPlot2.R')
library(amen)
################

################
# load data
load('../nigeriaMatList_acled_v7.rda') # loads yList object
load('../exoVars.rda') # load xNodeL, xDyadL

# focus on post 2000 data [few actors beforehand]
yrs = char(2000:2016)
yList = yList[yrs] ; xDyadL = xDyadL[yrs] ; xNodeL = xNodeL[yrs]
###############

###############
# make yList symmetric
yList = lapply(yList, function(y){
	diag(y) = NA ; ySymm = y + t(y) ; ySymm[ySymm>1] = 1
	return(ySymm) })
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

# parallelize model run
ameFits = lapply(1:length(designArrays), function(i){
	fit = ame_repL(
		Y=yList, Xdyad=designArrays[[i]]$dyadCovar,
		Xrow=designArrays[[i]]$senCovar,
		symmetric=TRUE, nvar=TRUE, R=2, 
		model='bin', intercept=TRUE, seed=6886,
		burn=10000, nscan=50000, odens=25,
		plot=FALSE, gof=TRUE, periodicSave=FALSE )
	return(fit) })
names(ameFits) = names(designArrays)
################

################
# quick trace plot
mcmcData = ameFits$base$BETA
varKey = data.frame(dirty=colnames(mcmcData),stringsAsFactors=FALSE)
varKey$clean = c(
	'Intercept',
	'Riots/Protests Against', 'Civilian Attacks', 'Geographic Spread',
	'Gov-Gov Actors','Post-Boko Haram', 'Election Year', 'Neighborhood Conflict')
varKey = varKey[c(6,2,3,4,7,8,5,1),]
ggsave(
	paramPlot2(mcmcData, varKey), 
	file='../figureA3.pdf', width=8,height=9)
################