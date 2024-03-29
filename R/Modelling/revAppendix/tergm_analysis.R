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

################
set.seed(6886)
if( !file.exists(paste0(pathResults, 'revAppendix/tergm.rda')) ){
	mod = mtergm(
		yNets ~ edges +
			edgecov(govDyCovar) + edgecov(bkDyCovar) +
			edgecov(elecDyCovar) + edgecov(spatDyCovar) +
			nodeocov('rioPro') + nodeocov('civVio') + nodeocov('geoSpread') +
			nodeicov('rioPro') + nodeicov('civVio') + nodeicov('geoSpread') +
			mutual + 
			gwesp(.5,fixed=TRUE)
		)
	save(mod, file=paste0(pathResults, 'revAppendix/tergm.rda')) }
load(paste0(pathResults, 'revAppendix/tergm.rda'))

# create summary table
res = cbind(
	mod@coef, mod@se, mod@pval
	)
colnames(res) = c('Estimate', 'Std. Error', 'P-value')
rownames(res) = c(
	'Intercept', 
	'Gov-Gov Actors$_{ij}$',
	'Post-Boko Haram Period$_{t}$',
	'Election Year$_{t}$',
	'Neighborhood Conflict$_{t}$',
	'Riots/Protests$_{i,t-1}$',
	'Violent Events Against Civilians$_{i,t-1}$',
	'Geographic Spread$_{i,t-1}$',	
	'Riots/Protests$_{j,t-1}$',
	'Violent Events Against Civilians$_{j,t-1}$',
	'Geographic Spread$_{j,t-1}$',
	'Mutuality', 
	'GWESP (0.5)'
	)
res = round(res[,-3], 2)
print.xtable(
	xtable(res, 
		align='lcc',
		caption='Posterior parameter estimate and standard errors from a TERGM analysis estimated via MCMC-MLE.',
		label='tab:tergm'		
		),
	include.rownames=TRUE, sanitize.text.function = identity,
	hline.after=c(0,0,1,nrow(res),nrow(res)),
	size='normalsize',
	file=paste0(pathResults, 'revAppendix/tergm.tex')
	)
################