################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R') }

#
loadPkg(c('lmtest', 'tidyr'))
source(paste0(fPth, 'postHelpers.R'))
################

################
# load data
load(paste0(pathResults, 'ameResults.rda'))
load(paste0(pathResults, 'glmResultsProbit.rda'))
load(paste0(pathResults, 'revAppendix/tergm.rda'))
###############

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
# glmBETA = gather(glmBETA[,-2], key='stat', value='glmValue', -var)

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
# ameBETA = gather(ameBETA, key='stat',value='ameValue', -var)

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
ggsave(coefCompare, file=paste0(pathGraphics, 'ame_v_glm_v_tergm.pdf'), width=5, height=7)
################