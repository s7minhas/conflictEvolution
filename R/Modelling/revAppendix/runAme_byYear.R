################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R') }

#
if(!'amen' %in% installed.packages()[,1]){
	loadPkg('devtools')
	devtools::install_github('s7minhas/amen') }
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
# set up model specs
subListArray = function(lA, vars, dims=2){
	if(dims==2){ return( lapply(lA, function(x){ x[,vars, drop=FALSE] }) ) }
	if(dims==3){ return( lapply(lA, function(x){ x[,,vars,drop=FALSE] }) ) } }
designArrays = list(
	NULL=list(senCovar=NULL, recCovar=NULL, dyadCovar=NULL), 
	base=list(
		senCovar=subListArray(xNodeL, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2),
		recCovar=subListArray(xNodeL, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2),
		dyadCovar=subListArray(xDyadL, c('govActor','elecYear','ngbrConfCount'), 3)
		# c('govActor', 'postBoko', 'elecYear', 'ngbrConfCount')
		# elecs = char(c(2003,2007,2011,2015))
		)
	)

## pull out necessary arrays
xdList = designArrays[[2]]$dyadCovar
xsList = designArrays[[2]]$senCovar
xrList = designArrays[[2]]$recCovar

# break up into smaller pds
pds = list(
	2000:2008, 
	2009:2016)
# pds = list(
# 	2000:2005, 2006:2011,
# 	2012:2016 )
pds = lapply(pds, char)

# parallelize model run
# if(!file.exists(paste0(pathResults, 'revAppendix/ameResults_byYear.rda'))){
	loadPkg(c('parallel','foreach'))
	cores=length(pds) ; cl=makeCluster(cores) ; registerDoParallel(cl)
	ameFits = foreach(i = 1:length(pds), .packages=c('amen')) %dopar% {

	# set up data
		fit = ame_repL(
			Y=yList[ pds[[i]] ], Xdyad=xdList[ pds[[i]] ],
			Xrow=xsList[ pds[[i]] ], Xcol=xrList[ pds[[i]] ],
			symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
			model='bin', intercept=TRUE, seed=6886,
			plot=FALSE, gof=TRUE, periodicSave=FALSE )
		return(fit)
	} ; stopCluster(cl)

	# save
	save(
		ameFits, 
		file=paste0(pathResults, 'revAppendix/ameResults_byYear.rda')
		)
# }
load(paste0(pathResults, 'revAppendix/ameResults_byYear.rda'))
################

################
# pull out param estimates
betaList = lapply(ameFits, function(x){x$'BETA'})

# create var labels
varKey = data.frame(dirty=colnames(ameFits[[1]]$BETA),stringsAsFactors=FALSE)
varKey$clean = c(
	'Intercept',
	'Riots/Protests Against (Sender)', 'Civilian Attacks (Sender)', 'Geographic Spread (Sender)',
	'Riots/Protests Against (Receiver)', 'Civilian Attacks (Receiver)', 'Geographic Spread (Receiver)',
	'Gov-Gov Actors', 'Election Year', 'Neighborhood Conflict')
################

################
# calc standardized reg coefs
for(i in 1:length(pds)){
	# calc sds
	sdY = sd(unlist(
		lapply(yList[ pds[[i]] ], function(x){
			diag(x) = NA ; return(c(x)) })), na.rm=TRUE)
	sd_xs = apply(do.call('rbind',xsList[ pds[[i]] ]), 2, sd)
	sd_xr = apply(do.call('rbind',xrList[ pds[[i]] ]), 2, sd)
	sd_xd1 = sd(unlist(
		lapply(xdList[ pds[[i]] ], function(x){
			x=x[,,1]; diag(x) = NA ; return(c(x)) })), na.rm=TRUE)
	sd_xd2 = sd(unlist(
		lapply(xdList[ pds[[i]] ], function(x){
			x=x[,,2]; diag(x) = NA ; return(c(x)) })), na.rm=TRUE)	
	sd_xd3 = sd(unlist(
		lapply(xdList[ pds[[i]] ], function(x){
			x=x[,,3]; diag(x) = NA ; return(c(x)) })), na.rm=TRUE)		
	sdX = c(1, sd_xs, sd_xr, sd_xd1, sd_xd2, sd_xd3)

	# sdz betas
	tmp=ameFits[[i]]$BETA
	for(var in 1:ncol(tmp)){ tmp[,var] = tmp[,var] * (sdX[var]/sdY)}
	ameFits[[i]]$BETA = tmp }
################

################
# time labels
labs=1:length(pds)
# labs = c(
# 	'2000-2005', '2006-2011',
# 	'2012-2016'
# 	)

# summarize param estimates
paramTime = lapply(1:length(ameFits), function(i){
	x = ameFits[[i]]
	ameBETA = t(apply(x$BETA, 2, summStats))
	colnames(ameBETA) = c('mean','lo95','hi95','lo90','hi90')
	ameBETA = data.frame(ameBETA, stringsAsFactors = FALSE)
	ameBETA$var = rownames(ameBETA) ; rownames(ameBETA) = NULL	
	ameBETA$var = varKey$clean[match(ameBETA$var, varKey$dirty)]
	ameBETA$pd = labs[i]
	return(ameBETA)
}) %>% do.call('rbind', .)
################	

################	
# viz
ggplot(paramTime, aes(x=factor(pd), y=mean)) +
	geom_point() +
	geom_linerange(aes(ymin=lo90,ymax=hi90), size=.6) +
	geom_linerange(aes(ymin=lo95,ymax=hi95), size=.2) +
	geom_hline(aes(yintercept=0)) +
	facet_wrap(~var, scales='free_y') +
	theme(
		panel.border = element_blank(),
		axis.ticks = element_blank(),
		axis.text.x = element_text(angle=45, hjust=1)
		)
################	