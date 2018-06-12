################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R') }
loadPkg('devtools') ; devtools::install_github('s7minhas/amen') ; library(amen)
################

################
# load data
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
load(paste0(pathData, 'exoVars.rda')) # load xNodeL, xDyadL

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
	NULL=list(senCovar=NULL, recCovar=NULL, dyadCovar=NULL), 
	base=list(
		senCovar=subListArray(xNodeL, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2),
		recCovar=subListArray(xNodeL, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2),
		dyadCovar=subListArray(xDyadL, c('govActor', 'postBoko', 'elecYear', 'ngbrConfCount'), 3)
		),
	rioProSep=list(
		senCovar=subListArray(xNodeL, c('riotsAgainst', 'protestsAgainst','vioCivEvents', 'groupSpread'), 2),
		recCovar=subListArray(xNodeL, c('riotsAgainst', 'protestsAgainst','vioCivEvents', 'groupSpread'), 2),
		dyadCovar=subListArray(xDyadL, c('govActor', 'postBoko', 'elecYear', 'ngbrConfCount'), 3)
		)
	)

# parallelize model run
loadPkg(c('parallel','foreach'))
cores=length(designArrays) ; cl=makeCluster(cores) ; registerDoParallel(cl)
ameFits = foreach(i = 1:length(designArrays), .packages=c('amen')) %dopar% {
	fit = ame_repL(
		Y=yList, Xdyad=designArrays[[i]]$dyadCovar,
		Xrow=designArrays[[i]]$senCovar,
		symmetric=TRUE, nvar=TRUE, R=2, 
		model='bin', intercept=TRUE, seed=6886,
		burn=10000, nscan=50000, odens=25,
		plot=FALSE, gof=TRUE, periodicSave=FALSE )
	return(fit)
} ; stopCluster(cl)
names(ameFits) = names(designArrays)
################

################
# save
save(
	ameFits, designArrays,
	file=paste0(pathResults, 'ameResults_symmetric.rda')
	)
################

################
#
source(paste0(fPth, 'paramPlot2.R'))

# quick trace plot
mcmcData = ameFits$base$BETA
varKey = data.frame(dirty=colnames(mcmcData),stringsAsFactors=FALSE)
varKey$clean = c(
	'Intercept',
	'Riots/Protests Against', 'Civilian Attacks', 'Geographic Spread',
	'Gov-Gov Actors','Post-Boko Haram', 'Election Year', 'Neighborhood Conflict')
varKey = varKey[c(6,2,3,4,7,8,5,1),]
ggsave(paramPlot2(mcmcData, varKey), file=paste0(pathGraphics, 'betaTrace_symmetric.pdf'), width=8,height=9)
################