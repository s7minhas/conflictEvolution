################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }

# add add'l libs
loadPkg('abind')
################

################
# load data
load(paste0(pathData,"exoVars.rda"))

# read in region file
aRegion = read.csv(paste0(pathData, 'nigeriaActorList.csv'))
aRegion$X = trim(char(aRegion$X))
noRegionData = setdiff(unique(unlist(lapply(xDyadL,rownames))), trim(aRegion$X))
################

################
# create dist mat
actors = c(unique(aRegion$X), noRegionData)
regionMat = matrix(NA, nrow=length(actors), ncol=length(actors), dimnames=list(actors,actors))
for(i in 1:nrow(regionMat)){ for(j in 1:ncol(regionMat)){ # :) lazy, not many actors #
	iLoc = 'hi' ; jLoc = 'bye'
	if(actors[i] %in% unique(aRegion$X)){
		iLoc = aRegion$zone[aRegion$X==actors[i]] }
	if(actors[j] %in% unique(aRegion$X)){
		jLoc = aRegion$zone[aRegion$X==actors[j]] }		
	if(iLoc == jLoc){ regionMat[i,j] = 1}
	if(iLoc != jLoc){ regionMat[i,j] = 0}
}}
diag(regionMat) = NA
gov = paste0(c('Military','Police'), ' Forces of Nigeria')
regionMat[gov,] = 1 ; regionMat[,gov] = 1
noRegInfo = c(
	'Christian Militia (Nigeria)',
	'Kanberi Ethnic Militia (Nigeria)',
	'Muslim Militia (Nigeria)',
	'Shiite Muslim Militia (Nigeria)',
	'Sunni Muslim Militia (Nigeria)',
	'Vigilante Militia (Nigeria)',
	noRegionData
	)
regionMat[noRegInfo,] = NA ; regionMat[,noRegInfo] = NA
################

################
# merge into dyad covar list
for(t in 1:length(xDyadL)){
	xActors = rownames(xDyadL[[t]])
	xDyadL[[t]]=abind(xDyadL[[t]], sameRegion=regionMat[xActors,xActors], along=3)
}
################

################
# save
save(xDyadL, xNodeL, file = paste0(pathData,"exoVars.rda"))
################