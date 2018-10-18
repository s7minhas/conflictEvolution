################
# workspace
source('setup.R')
loadPkg('devtools')
devtools::install_github('s7minhas/amen', ref='pa2018_version')
library(amen)
################

################
# load data
load('nigeriaMatList_acled_v7.rda') # loads yList object
load('exoVars.rda') # load xNodeL, xDyadL

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

# run base model
ameFits = lapply(1:length(designArrays), function(i){
	fit = ame_repL(
		Y=yList, Xdyad=designArrays[[i]]$dyadCovar,
		Xrow=designArrays[[i]]$senCovar, Xcol=designArrays[[i]]$recCovar,
		symmetric=FALSE, rvar=TRUE, cvar=TRUE, R=2, 
		model='bin', intercept=TRUE, seed=6886,
		burn=10000, nscan=50000, odens=25,
		plot=FALSE, gof=TRUE, periodicSave=FALSE )
	return(fit)
})
names(ameFits) = names(designArrays)
################

################
# save
save(
	ameFits, designArrays,
	file='ameResults.rda'
	)
################

################
# functions to help with viz of results
source('postHelpers.R')
source('paramPlot2.R')
################

################
# load data
load('ameResults.rda') # load AME mod results

# quick trace plot
mcmcData = ameFits$base$BETA
varKey = data.frame(dirty=colnames(mcmcData),stringsAsFactors=FALSE)
varKey$clean = c(
	'Intercept',
	'Riots/Protests Against (Sender)', 'Civilian Attacks (Sender)', 'Geographic Spread (Sender)',
	'Riots/Protests Against (Receiver)', 'Civilian Attacks (Receiver)', 'Geographic Spread (Receiver)',
	'Gov-Gov Actors','Post-Boko Haram', 'Election Year', 'Neighborhood Conflict')
varKey = varKey[c(9,2,5,3,6,4,7,10,11,8,1),]
################

################
# bring in covar data in rect format
load('glmResults.rda')

# calc standardized reg coefs
sdY = sd(gfitFullSpec$data$value)
xVars = colnames(ameFits$base$BETA) %>% gsub('.dyad','',.) %>% .[-1]
sdX = apply(gfitFullSpec$data[,xVars], 2, sd)

#
tmp=ameFits$base$BETA[,2:ncol(ameFits$base$BETA)]
for(i in 1:ncol(tmp)){ tmp[,i] = tmp[,i] * (sdX[i]/sdY)}
ameFits$base$BETA[,2:ncol(ameFits$base$BETA)] = tmp
################

################
# gather beta estimates from ame fullSpec
ameBETA = t(apply(ameFits$base$BETA, 2, summStats))
colnames(ameBETA) = c('mean','lo95','hi95','lo90','hi90')
ameBETA = data.frame(ameBETA, stringsAsFactors = FALSE)
ameBETA$var = rownames(ameBETA) ; rownames(ameBETA) = NULL

# drop extras and unnecessary params
ameBETA = ameBETA[-which(ameBETA$var %in% c('intercept')),]

# classify vars by type
ameBETA$type = unlist(lapply(strsplit(ameBETA$var,'.',fixed=TRUE), function(x){x[2]}))
ameBETA$var = gsub('.','',gsub('dyad|row|col','',ameBETA$var),fixed=TRUE)

# cleanup
ameBETA$varClean = c(
	'Riots/Protests$_{i,t-1}$',
	'Violent Events\nAgainst Civilians$_{i,t-1}$',
	'Geographic Spread$_{i,t-1}$',
	'Riots/Protests$_{j,t-1}$',
	'Violent Events\nAgainst Civilians$_{j,t-1}$',
	'Geographic Spread$_{j,t-1}$',
	'Gov-Gov Actors$_{ij}$',
	'Post-Boko\nHaram Period$_{t}$',
	'Election Year$_{t}$',
	'Neighborhood Conflict$_{t}$'
	)
ameBETA$typeClean = c(
	rep('... Sender-Level Covariate(s)',3),
	rep('... Receiver-Level Covariate(s)',3),
	rep('Parameter Estimates for Country-Level Covariate(s)',4)
	)

# clean up var labels
ameBETA$typeClean[ameBETA$var=='govActor'] = '... Dyad-Level Covariate(s)'

# add sig info
ameBETA = getSigVec(ameBETA)
################

################
# viz
cleanVars = ameBETA$varClean
ameBETA$typeClean = factor(ameBETA$typeClean, levels=unique(ameBETA$typeClean)[c(4,3,1,2)])
ggCoef=ggplot(ameBETA, aes(x=varClean, y=mean, color=sig)) +
	facet_wrap(~typeClean,ncol=1,scales='free_y') + 
	geom_hline(aes(yintercept=0), linetype=2, color = "black") + 
	geom_point(size=4) + 
	geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1.5) + 
	geom_linerange(aes(ymin=lo95,ymax=hi95),alpha = 1, size = .5) +	
	scale_colour_manual(values = coefp_colors, guide=FALSE) +
	scale_x_discrete('', labels=TeX(rev(cleanVars))) +	
	ylab(TeX('$\\beta_{p} \\times \\frac{\\sigma_{x_{p}}}{\\sigma_{y}}$')) +
	coord_flip() + 
	theme(
		legend.position='none', legend.title=element_blank(),
		panel.border=element_blank(),
		axis.ticks=element_blank(),
		axis.text.y=element_text(hjust=0),
		strip.text.x = element_text(size = 9, color='white',
			angle=0, hjust=.95),
		strip.background = element_rect(fill = "#525252", color='#525252')				
	)
ggsave( ggCoef, file='figure4_top.pdf', width=7, height=6 )
################

################
# labs
vcKey = data.frame(dirty=colnames(ameFits$base$VC)[-ncol(ameFits$base$VC)], stringsAsFactors = FALSE) 
vcKey$clean = c(
	'Within-Sender\nVariance ($\\sigma_{a]^{2}$)',
	'Sender-Receiver\nCovariance ($\\sigma_{ab]$)',
	'Within-Receiver\nVariance ($\\sigma_{b]^{2}$)',
	'Reciprocity ($\\rho$)'
	)
################

################
# org data
vc = t(apply(ameFits$base$VC[,-ncol(ameFits$base$VC)], 2, summStats))
colnames(vc) = c('mean','lo95','hi95','lo90','hi90')
vc = data.frame(vc, stringsAsFactors = FALSE)
vc$var = rownames(vc) ; rownames(vc) = NULL
vc$varClean = vcKey$clean[match(vc$var, vcKey$dirty)]
vc$varClean = factor(vc$varClean, levels=rev(vcKey$clean[c(1,3,2,4)]))
################

################
# plot
vc$varClean = factor(vc$varClean, levels=vcKey$clean[c(1,3,2,4)])
vc$sig = 'Positive'
vc$bigLab = 'SRRM Parameters'
ggVC = ggplot(vc, aes(x=varClean, y=mean, color=sig)) + 
	geom_hline(aes(yintercept=0), linetype=2, color = "black") + 
	geom_point(size=2.5) + 
	geom_linerange(aes(ymin=lo95,ymax=hi95), linetype=1, size=.5) + 
	geom_linerange(aes(ymin=lo90,ymax=hi90), linetype=1, size=1.5) + 
	scale_color_manual(values=coefp_colors) + 
	facet_wrap(~bigLab) + 
	scale_x_discrete('',labels=TeX(levels(rev(vc$varClean)))) + ylab('') +
	theme(
		legend.position = 'none',
		axis.ticks=element_blank(),
		panel.border=element_blank(),
		axis.text.x=element_text(vjust=-1),
		strip.text.x = element_text(size = 10, color='white',
			angle=0, hjust=.03),
		strip.background = element_rect(fill = "#525252", color='#525252')
		)
ggsave(ggVC, file='figure4_bottom.pdf', width=8, height=2)
################