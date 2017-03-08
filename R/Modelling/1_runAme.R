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

################
# Set up fitFullSpec model

# separate nodal into row and col [unnecessary in this case]
xNodeL = lapply(xNodeL, function(x){
	x = cbind(x, x[,'riotsAgainst'] + x[,'protestsAgainst'])
	colnames(x)[ncol(x)] = 'rioProContra' ; return(x) })

xRowL = lapply(xNodeL, function(x){
	x=x[,c('rioProContra','vioCivEvents'),drop=FALSE]	
	return(x) })
xColL = lapply(xNodeL, function(x){
	x=x[,c('rioProContra','vioCivEvents'),drop=FALSE]
	return(x) })

# dyadic covar specs
xDyadL_noDist = lapply(xDyadL, function(x){
  x=x[,,c('govActor','postBoko','elecYear'),drop=FALSE]
  return(x) })

# run models
# fit=ame_repL(
# 	Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL, 
# 	symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
# 	model='bin', intercept=TRUE, seed=6886,
# 	burn=50000, nscan=25000, odens=25, 
# 	plot=FALSE, gof=TRUE, periodicSave=FALSE
# 	)

fitFullSpec_nigLevel=ame_repL(
	Y=yList, Xdyad=xDyadL, Xrow=xRowL, Xcol=xColL,
	symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
	model='bin', intercept=TRUE, seed=6886,
	burn=100000, nscan=500000, odens=25, 
	plot=FALSE, gof=TRUE, periodicSave=FALSE
	)

fitFullSpec_noDist_nigLevel=ame_repL(
	Y=yList, Xdyad=xDyadL_noDist, Xrow=xRowL, Xcol=xColL,
	symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
	model='bin', intercept=TRUE, seed=6886,
	burn=50000, nscan=100000, odens=25, 
	plot=FALSE, gof=TRUE, periodicSave=FALSE
)

fitFullSpec_nigLevel = fitFullSpec
fitFullSpec_noDist_nigLevel = fitFullSpec_noDist
# load(paste0(pathResults, 'ameResults.rda'))

################

# ################
# # save
# save(
# 	fit, fitFullSpec, fitFullSpec_noDist,
# 	yList, xNodeL, xRowL, xColL,
# 	xDyadL, xDyadL_noDist,
# 	file=paste0(pathResults, 'ameResults.rda')
# 	)
# ################