################
# workspace
source('../main/setup.R')
source('../main/paramPlot2.R')
library(amen)
################

################
# load data
load('../main/nigeriaMatList_acled_v7.rda') # loads yList object
load('../main/exoVars.rda') # load xNodeL, xDyadL

# focus on post 2000 data [few actors beforehand]
yrs = char(2000:2016)
yList = yList[yrs] ; xDyadL = xDyadL[yrs] ; xNodeL = xNodeL[yrs]


yList = lapply(yList, function(y){
	toDrop = grep('Forces of Nigeria', rownames(y), fixed=TRUE)
	y=y[-toDrop,] ; y=y[,-toDrop] ; return(y)
})

xDyadL = lapply(xDyadL, function(y){
	toDrop = grep('Forces of Nigeria', rownames(y), fixed=TRUE)
	y=y[-toDrop,,] ; y=y[,-toDrop,] ; return(y)
})

xNodeL = lapply(xNodeL, function(y){
	toDrop = grep('Forces of Nigeria', rownames(y), fixed=TRUE)
	y=y[-toDrop,] ; return(y)
})
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
		dyadCovar=subListArray(xDyadL, c('postBoko', 'elecYear', 'ngbrConfCount'), 3)
		)
	)

# parallelize model run
ameFits = lapply(1:length(designArrays), function(i){
	fit = ame_repL(
		Y=yList, Xdyad=designArrays[[i]]$dyadCovar,
		Xrow=designArrays[[i]]$senCovar, Xcol=designArrays[[i]]$recCovar,
		symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
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
	'Riots/Protests Against (Sender)', 'Civilian Attacks (Sender)', 'Geographic Spread (Sender)',
	'Riots/Protests Against (Receiver)', 'Civilian Attacks (Receiver)', 'Geographic Spread (Receiver)',
	'Post-Boko Haram', 'Election Year', 'Neighborhood Conflict')
varKey = varKey[c(8,2,5,3,6,4,7,9,10,1),]
ggsave(
	paramPlot2(mcmcData, varKey), 
	file='floats/figureA4.pdf', width=8,height=9)
################