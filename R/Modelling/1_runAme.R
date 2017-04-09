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

subListArray = function(lA, vars, dims=2){
	if(dims==2){ return( lapply(lA, function(x){ x[,vars, drop=FALSE] }) ) }
	if(dims==3){ return( lapply(lA, function(x){ x[,,vars,drop=FALSE] }) ) } }
modSpecs = list(
	NULL=list(senCovar=NULL, recCovar=NULL, dyadCovar=NULL), 
	base=list(senCovar=subListArray(xNodeL,c('riotsProtestsAgainst')))
	)


'riotsProtestsAgainst', 'vioCivEvents'

'govActor', 'postBoko', 'elecYear'
'govActor', 'postBoko', 'elecYear', 'ngbrConfCount'


fitFullSpec=ame_repL(
	Y=yList, 
	Xdyad=subListArray(xDyadL, c('govActor', 'postBoko', 'elecYear', 'ngbrConfCount'), 3), 
	Xrow=subListArray(xNodeL, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2), 
	Xcol=subListArray(xNodeL, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2),
	symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
	model='bin', intercept=TRUE, seed=6886,
	# burn=100000, nscan=500000, odens=25, 
	burn=1000, nscan=5000, odens=25, 
	plot=FALSE, gof=TRUE, periodicSave=FALSE
	)
ggsave(paramPlot(fitFullSpec$BETA), file='~/Desktop/tmp.png') ; system('open ~/Desktop/tmp.png')
ggsave(paramPlot(fitFullSpec$VC), file='~/Desktop/tmp2.png') ; system('open ~/Desktop/tmp2.png')

# control for internal spatial effects
aRegion = read.csv(paste0(pathData, 'nigeriaActorList.csv'))
noRegInfo = c(
	'Christian Militia (Nigeria)',
	'Kanberi Ethnic Militia (Nigeria)',
	'Muslim Militia (Nigeria)',
	'Shiite Muslim Militia (Nigeria)',
	'Sunni Muslim Militia (Nigeria)',
	'Vigilante Militia (Nigeria)',
	setdiff(unique(unlist(lapply(xDyadL,rownames))), trim(char(aRegion$X)))
	) ; rm(aRegion)
ySmall = lapply(yList, function(y){
	toKeep = setdiff(rownames(y), noRegInfo) ; return(y[toKeep,toKeep]) })
nodeSmall = lapply(xNodeL, function(y){
	toKeep = setdiff(rownames(y), noRegInfo) ; return(y[toKeep,]) })
dyadSmall = lapply(xDyadL, function(y){
	toKeep = setdiff(rownames(y), noRegInfo) ; return(y[toKeep,toKeep,]) })

fitFullSpec_sameRegion=ame_repL(
	Y=ySmall, 
	Xdyad=subListArray(dyadSmall, c('govActor', 'postBoko', 'elecYear', 'ngbrConfCount','sameRegion'), 3), 
	Xrow=subListArray(nodeSmall, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2), 
	Xcol=subListArray(nodeSmall, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2),
	symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
	model='bin', intercept=TRUE, seed=6886,
	# burn=100000, nscan=500000, odens=25, 
	burn=1000, nscan=5000, odens=25, 
	plot=FALSE, gof=TRUE, periodicSave=FALSE
	)

ggsave(paramPlot(fitFullSpec_sameRegion$BETA), file='~/Desktop/tmpa.png') ; system('open ~/Desktop/tmpa.png')
ggsave(paramPlot(fitFullSpec_sameRegion$VC), file='~/Desktop/tmpa2.png') ; system('open ~/Desktop/tmpa2.png')

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