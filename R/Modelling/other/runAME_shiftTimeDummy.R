################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
library(amen)
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
xRowL = lapply(xNodeL, function(x){
	x=x[,c('riotsAgainst','vioCivEvents'),drop=FALSE]	
	x[c('Military Forces of Nigeria','Police Forces of Nigeria'),] = 0
	return(x) })
xColL = lapply(xNodeL, function(x){
	x=x[,c('riotsAgainst','vioCivEvents'),drop=FALSE]	
	x[c('Military Forces of Nigeria','Police Forces of Nigeria'),] = 0
	return(x) })

# mod xDyadL
actors=unique(unlist(lapply(yList, rownames)))
govActors=c('Military Forces of Nigeria','Police Forces of Nigeria')

postBokoMods=paste0('postBoko',c(paste0('_m',1:5),'',paste0('_a',1:5)))
xDyadL = lapply(1:length(yList), function(t){
	actors = rownames( yList[[t]] )
	yr = num(names(yList)[t])
	xArr = array(0,dim=c(length(actors),length(actors),length(postBokoMods)+1),
		dimnames=list(actors,actors,c('govActor', postBokoMods )))
	xArr[which(rownames(xArr) %in% govActors),which(colnames(xArr) %in% govActors),'govActor'] = 1
	if(yr>2003){xArr[,,'postBoko_m5']=1} # boko enters network in 2009	
	if(yr>2004){xArr[,,'postBoko_m4']=1} # boko enters network in 2009
	if(yr>2005){xArr[,,'postBoko_m3']=1} # boko enters network in 2009
	if(yr>2006){xArr[,,'postBoko_m2']=1} # boko enters network in 2009
	if(yr>2007){xArr[,,'postBoko_m1']=1} # boko enters network in 2009
	if(yr>2008){xArr[,,'postBoko']=1} # boko enters network in 2009	
	if(yr>2009){xArr[,,'postBoko_a1']=1} # boko enters network in 2009
	if(yr>2010){xArr[,,'postBoko_a2']=1} # boko enters network in 2009
	if(yr>2011){xArr[,,'postBoko_a3']=1} # boko enters network in 2009
	if(yr>2012){xArr[,,'postBoko_a4']=1} # boko enters network in 2009	
	if(yr>2013){xArr[,,'postBoko_a5']=1} # boko enters network in 2009		
	for(p in 1:dim(xArr)[3]){ diag(xArr[,,p])=NA }
	return(xArr)
}) ; names(xDyadL) = names(yList)

#
xDyadL_mods = lapply(postBokoMods, function(t){
	lapply(xDyadL, function(x){ x[,,c('govActor',t)] })
})
names(xDyadL_mods) = postBokoMods

# run model
loadPkg(c('doParallel', 'foreach'))
cl=makeCluster(5)
registerDoParallel(cl)
fitPostBokoMods <- foreach(
	ii=1:length(xDyadL_mods), 
	.packages=c('amen')
	) %dopar% {

	# mod
	mod=ame_repL( Y=yList,
		Xdyad=xDyadL_mods[[ii]], Xrow=xRowL, Xcol=xColL,
		symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
		model='bin', intercept=TRUE, seed=6886,
		burn=10000, nscan=20000, odens=10, 
		plot=FALSE, gof=TRUE, periodicSave=FALSE )
	return(mod)
}
stopCluster(cl)
################

################
# save
save(
	fitPostBokoMods,
	yList, xNodeL, xDyadL, xRowL, xColL,
	file=paste0(pathResults, 'ame_shiftTimeDummy.rda')
	)
################