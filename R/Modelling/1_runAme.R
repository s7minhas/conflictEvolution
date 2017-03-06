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
# NULL model
fit=ame_repL(
	Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL, 
	symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
	model='bin', intercept=TRUE, seed=6886,
	burn=50000, nscan=25000, odens=25, 
	plot=FALSE, gof=TRUE, periodicSave=FALSE
	)
################

################
# Set up fitFullSpec model
# separate nodal into row and col [unnecessary in this case]
xNodeL = lapply(xNodeL, function(x){
	x = cbind(x, x[,'riotsAgainst'] + x[,'protestsAgainst'])
	colnames(x)[ncol(x)] = 'rioProContra' ; return(x)
})

xRowL = lapply(xNodeL, function(x){
	x=x[,c('rioProContra','vioCivEvents', 'groupSpread'),drop=FALSE]	
	return(x) })
xColL = lapply(xNodeL, function(x){
	x=x[,c('rioProContra','vioCivEvents', 'groupSpread'),drop=FALSE]
	return(x) })

# run model
fitFullSpec=ame_repL(
	Y=yList, Xdyad=xDyadL, Xrow=xRowL, Xcol=xColL,
	symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
	model='bin', intercept=TRUE, seed=6886,
	burn=25000, nscan=100000, odens=25, 
	plot=FALSE, gof=TRUE, periodicSave=FALSE
	)
################

################
# save
save(
	fit, fitFullSpec,
	yList, xNodeL, xDyadL, xRowL, xColL,
	file=paste0(pathResults, 'ameResults.rda')
	)
################