################
# workspace
source('../setup.R')
detach("package:igraph", unload=TRUE)
loadPkg(c('btergm','xtable'))
################

################
# load data
load('../nigeriaMatList_acled_v7.rda') # loads yList object
load('../exoVars.rda') # load xNodeL, xDyadL

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
mod = mtergm(
	yNets ~ edges +
		edgecov(govDyCovar) + edgecov(bkDyCovar) +
		edgecov(elecDyCovar) + edgecov(spatDyCovar) +
		nodeocov('rioPro') + nodeocov('civVio') + nodeocov('geoSpread') +
		nodeicov('rioPro') + nodeicov('civVio') + nodeicov('geoSpread') +
		mutual + 
		gwesp(.5,fixed=TRUE)
	)
################

################
# load other models
load('../ameResults.rda')
load('../glmResultsProbit.rda')

# bring in some helpers
loadPkg(c('lmtest', 'tidyr'))
source('../postHelpers.R')
################

################
mcmcData = ameFits$base$BETA
varKey = data.frame(dirty=colnames(mcmcData),stringsAsFactors=FALSE)
varKey$clean = c(
	'Intercept',
	'Riots/Protests (Sender)', 'Violent Events\nAgainst Civilians (Sender)', 'Geographic Spread (Sender)',
	'Riots/Protests (Receiver)', 'Violent Events\nAgainst Civilians (Receiver)', 'Geographic Spread (Receiver)',
	'Gov-Gov Actors','Post-Boko Haram', 'Election Year', 'Neighborhood Conflict')
################

################
# org glm results
glmBETA = data.frame(
	mean=coeftest(gfitFullSpec)[,1],
	sd=coeftest(gfitFullSpec)[,2] )
glmBETA$var = char(rownames(coeftest(gfitFullSpec)))
glmBETA$var[glmBETA$var=='(Intercept)'] = 'intercept'
glmBETA$var[glmBETA$var=='govActor'] = 'govActor.dyad'
glmBETA$var[glmBETA$var=='postBoko'] = 'postBoko.dyad'
glmBETA$var[glmBETA$var=='elecYear'] = 'elecYear.dyad'
glmBETA$var[glmBETA$var=='ngbrConfCount'] = 'ngbrConfCount.dyad'
glmBETA = glmBETA %>% getCIVecs() %>% getSigVec()
glmBETA$model = 'GLM'

# org tergm results
tergmBETA = data.frame( mean=mod@coef, sd=mod@se )
tergmBETA$var = c(
	'intercept', 'govActor.dyad', 'postBoko.dyad', 'elecYear.dyad',
	'ngbrConfCount.dyad', 
	'riotsProtestsAgainst.col', 'vioCivEvents.col', 'groupSpread.col',
	'riotsProtestsAgainst.row', 'vioCivEvents.row', 'groupSpread.row',
	'Reciprocity', 'GWESP (0.5)'
	)
tergmBETA = tergmBETA %>% getCIVecs() %>% getSigVec()
tergmBETA$model = 'TERGM'
varKey = rbind(varKey,
	c('Reciprocity', 'Reciprocity'),
	c('GWESP (0.5)', 'GWESP') )

# org ame results
ameBETA = t(apply(ameFits$base$BETA, 2, summStats))
colnames(ameBETA) = c('mean','lo95','hi95','lo90','hi90')
ameBETA = data.frame(ameBETA, stringsAsFactors = FALSE)
ameBETA$var = rownames(ameBETA) ; rownames(ameBETA) = NULL
ameBETA = getSigVec(ameBETA)
ameBETA$model = 'AME'

# merge
ggBETA = rbind(ameBETA, glmBETA[,names(ameBETA)], tergmBETA[,names(ameBETA)])

# clean up var names
ggBETA$varClean = varKey$clean[match(ggBETA$var, varKey$dirty)]
ggBETA$varClean = factor(ggBETA$varClean, levels=rev(varKey$clean))
################

################
posDodge=.75
coefCompare = ggplot(ggBETA, aes(x=varClean, y=mean, color=sig, group=model, shape=model)) +
	geom_hline(aes(yintercept=0), linetype=2, color = "black") + 
	geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1.5,
		position=position_dodge(width = posDodge)) + 
	geom_linerange(aes(ymin=lo95,ymax=hi95),alpha = 1, size = .5,
		position=position_dodge(width = posDodge)) +	
	geom_point(size=3, position=position_dodge(width = posDodge)) + 	
	scale_colour_manual(values = coefp_colors, guide=FALSE) +
	labs(x='',y='') +
	coord_flip() +
	theme(
		legend.position = 'top',
		legend.title=element_blank(),
		panel.border=element_blank(),
		axis.ticks=element_blank()
	)	
ggsave(coefCompare, 
	file='figureA9.pdf', width=5, height=7)
################