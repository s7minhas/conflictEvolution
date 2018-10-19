################
# workspace
source('../main/setup.R')
library(amen)
source('../main/postHelpers.R')
################

################
# load data
load('../main/nigeriaMatList_acled_v7.rda') # loads yList object
load('../main/exoVars.rda') # load xNodeL, xDyadL

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
		recCovar=subListArray(xNodeL, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2)
		)
	)

## pull out necessary arrays
xdList = designArrays[[2]]$dyadCovar
xsList = designArrays[[2]]$senCovar
xrList = designArrays[[2]]$recCovar

# break up into smaller pds
pds = list(
	2000:2002, 2003:2005, 
	2006:2008, 2009:2011,
	2012:2014, 2015:2016
	)
pds = lapply(pds, char)

# parallelize model run
cores=min(length(pds),5) ; cl=makeCluster(cores) ; registerDoParallel(cl)
ameFits = foreach(i = 1:length(pds), .packages=c('amen')) %dopar% {

	fit = ame_repL(
		Y=yList[ pds[[i]] ], 
		Xrow=xsList[ pds[[i]] ], Xcol=xrList[ pds[[i]] ],
		model='bin', intercept=TRUE, seed=6886, 
		plot=FALSE, gof=TRUE, periodicSave=FALSE )
	return(fit)
} ; stopCluster(cl)
################

################
# pull out param estimates
betaList = lapply(ameFits, function(x){x$'BETA'})

# create var labels
varKey = data.frame(dirty=colnames(ameFits[[1]]$BETA),stringsAsFactors=FALSE)
varKey$clean = c(
	'Intercept',
	'Riots/Protests$_{i,t-1}$', 
	'Violent Events Against Civilians$_{i,t-1}$', 
	'Geographic Spread$_{i,t-1}$',
	'Riots/Protests$_{j,t-1}$', 
	'Violent Events Against Civilians$_{j,t-1}$', 
	'Geographic Spread$_{j,t-1}$'
	)
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
	sdX = c(.1, sd_xs, sd_xr)

	# sdz betas
	tmp=ameFits[[i]]$BETA
	for(var in 1:ncol(tmp)){ tmp[,var] = tmp[,var] * (sdX[var]/sdY)}
	ameFits[[i]]$BETA = tmp }
################

################
# time labels
labs = unlist(lapply(pds, function(x){
  paste(x[1], x[length(x)],sep='-') }))

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
facet_labeller = function(string){ TeX(string) }
varOrder = varKey$clean[c(2:nrow(varKey),1)]
paramTime$var = factor(paramTime$var, levels=varOrder)
gg=ggplot(paramTime, aes(x=factor(pd), y=mean)) +
	geom_point() +
	geom_linerange(aes(ymin=lo90,ymax=hi90), size=.3) +
	geom_linerange(aes(ymin=lo95,ymax=hi95), size=.1) +
	geom_hline(aes(yintercept=0), linetype=2, color = "black") +
	facet_wrap(~var, scales='free_y', ncol=3,
		labeller=as_labeller(facet_labeller, default = label_parsed)) +
	labs(x='', y='') +
	theme(
		panel.border = element_blank(),
		axis.ticks = element_blank(),
		axis.text.x = element_text(angle=45, hjust=1,size=6),
		axis.text.y=element_text(hjust=0, size=6),
		strip.text.x = element_text(size = 9, color='white',
			angle=0, hjust=.05),
		strip.background = element_rect(fill = "#525252", color='#525252')
		)
ggsave(gg, file='floats/figureA8.pdf', width=8, height=6)
################	