################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R') }

#
detach("package:igraph", unload=TRUE)
loadPkg('btergm')
loadPkg('xtable')
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
		)
	)
################

################
#
actorSet <- sort(unique(unlist( lapply(yList, rownames) )))
n <- length(actorSet)

# 
arrayObj<-listToArray(
	actorSet, yList, 
	designArrays$base$dyadCovar, 
	designArrays$base$senCovar, 
	designArrays$base$recCovar
	)
Y<-arrayObj$Y ; Xdyad<-arrayObj$Xdyad
Xrow<-arrayObj$Xrow ; Xcol<-arrayObj$Xcol
rm(arrayObj)

# prep for btergm
yMats = lapply(1:dim(Y)[3], function(t){
	#
	ySlice = Y[,,t]
	ySlice[is.na(ySlice)] = 10
	diag(ySlice) = 0
	return( ySlice ) })

#
prepCovar = function(data, var){
	covar = lapply(1:dim(data)[4], function(t){
		xSlice = data[,,var,t]
		xSlice[is.na(xSlice)] = NA
		diag(xSlice) = 0
		return( xSlice ) })
	return(covar) }
govDyCovar = prepCovar(Xdyad, 'govActor')
bkDyCovar = prepCovar(Xdyad, 'postBoko')
elecDyCovar = prepCovar(Xdyad, 'elecYear')
spatDyCovar = prepCovar(Xdyad, 'ngbrConfCount')

#
yMats = handleMissings(yMats, na=10, method='remove')
govDyCovar = handleMissings(govDyCovar, na=NA, method='remove')
bkDyCovar = handleMissings(bkDyCovar, na=NA, method='remove')
elecDyCovar = handleMissings(elecDyCovar, na=NA, method='remove')
spatDyCovar = handleMissings(spatDyCovar, na=NA, method='remove')

#
yNets = lapply(1:length(yMats), function(t){
	#
	yNet = network(yMats[[t]])

	#
	actors = network.vertex.names(yNet)
	
	yNet = set.vertex.attribute(yNet, "rioPro", Xrow[actors,'riotsProtestsAgainst',t])
	yNet = set.vertex.attribute(yNet, "civVio", Xrow[actors,'vioCivEvents',t])
	yNet = set.vertex.attribute(yNet, "geoSpread", Xrow[actors,'groupSpread',t])

	#
	return(yNet) })
################