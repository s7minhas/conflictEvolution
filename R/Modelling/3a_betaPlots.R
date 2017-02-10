################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
source(paste0(fPth, 'postHelpers.R'))
source(paste0(fPth, 'paramPlot2.R'))
################

################
# load data
load(paste0(pathResults, 'ameResults.rda')) # load AME mod results
load(paste0(pathResults, 'glmResults.rda')) # load GLM mod results

# quick trace plot
mcmcData = fitFullSpec$BETA
varKey = data.frame(dirty=colnames(mcmcData),stringsAsFactors=FALSE)
varKey$clean = c(
	'Intercept',
	'Riots Against (Sender)', 'Civilian Attacks (Sender)',
	'Riots Against (Receiver)', 'Civilian Attacks (Receiver)', 'Gov-Gov Actors','Post-Boko Haram')
varKey = varKey[c(7,2,4,3,5,6,1),]
ggsave(paramPlot2(mcmcData, varKey), file=paste0(pathGraphics, 'betaTrace.pdf'), width=8,height=7)
################

################
# calc standardized reg coefs
sdY = sd(gfitFullSpec$data$value)
xVars = colnames(fitFullSpec$BETA) %>% gsub('.dyad','',.) %>% .[-1]
sdX = apply(gfitFullSpec$data[,xVars], 2, sd)

#
tmp=fitFullSpec$BETA[,2:ncol(fitFullSpec$BETA)]
for(i in 1:ncol(tmp)){ tmp[,i] = tmp[,i] * (sdX[i]/sdY)}
fitFullSpec$BETA[,2:ncol(fitFullSpec$BETA)] = tmp
################

################
# gather beta estimates from ame fullSpec
ameBETA = t(apply(fitFullSpec$BETA, 2, summStats))
colnames(ameBETA) = c('mean','lo95','hi95','lo90','hi90')
ameBETA = data.frame(ameBETA, stringsAsFactors = FALSE)
ameBETA$var = rownames(ameBETA) ; rownames(ameBETA) = NULL

# drop extras and unnecessary params
ameBETA = ameBETA[-which(ameBETA$var %in% c('intercept','govActor.dyad')),]
ameBETA$var[ameBETA$var=='postBoko.dyad'] = 'postBoko'

# combine and cleanup
dirtyVars=c('postBoko',paste0('riotsAgainst.',c('row','col')),paste0('vioCivEvents.',c('row','col')))
cleanVars=c(
	'Post-Boko\nHaram Period$_{t}$',
	'Number of\nRiots/Protests\nAgainst Sender$_{i,t-1}$',
	'Number of\nRiots/Protests\nAgainst Receiver$_{j,t-1}$',
	'Number of\nViolent Events\nAgainst Civilians\nCommitted\nby Sender$_{i,t-1}$',
	'Number of\nViolent Events\nAgainst Civilians\nCommitted\nby Receiver$_{j,t-1}$'
	)
ameBETA$varClean=NA; for(i in 1:length(dirtyVars)){ameBETA$varClean[ameBETA$var==dirtyVars[i]]=cleanVars[i]}
ameBETA$varClean = factor(ameBETA$varClean, levels=rev(cleanVars))

# add sig info
ameBETA = getSigVec(ameBETA)
################

################
# create plot including glmBETA estimates as well
glmBETA = data.frame(mean=coef(gfitFullSpec),sd=sqrt(diag(vcov(gfitFullSpec))))
# drop extras and unnecessary params
glmBETA$var = rownames(glmBETA) ; rownames(glmBETA) = NULL
glmBETA = glmBETA[-which(glmBETA$var %in% c('(Intercept)','govActor')),]
sdX2 = sdX[-5] ; glmBETA = glmBETA[match(names(sdX2), glmBETA$var),]
glmBETA$mean = glmBETA$mean*(sdX2/sdY) ; glmBETA$sd = glmBETA$sd*(sdX2/sdY)

# get cis & add sig info
glmBETA = getCIVecs(glmBETA) ; glmBETA = getSigVec(glmBETA)

# combine with ame
glmBETA$mod = 'GLM' ; glmBETA = glmBETA[,-which('sd' == names(glmBETA))]
glmBETA$varClean = ameBETA$varClean[match(glmBETA$var, ameBETA$var)]
ameBETA$mod = 'AME   ' ; glmBETA = glmBETA[,names(ameBETA)]
beta = rbind(ameBETA, glmBETA)

# plot
posDodge = .75
beta = beta[beta$mod=='AME   ',]
beta$mod = 'AME'
ggCoef=ggplot(beta, aes(x=varClean, y=mean, color=sig, group=mod)) + 
	geom_hline(aes(yintercept=0), linetype=2, color = "black") + 
	geom_point(aes(shape=mod), size=4, position=position_dodge(width = posDodge)) + 
	geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1.5, position=position_dodge(width = posDodge)) + 
	geom_linerange(aes(ymin=lo95,ymax=hi95),alpha = 1, size = .5, position=position_dodge(width = posDodge)) +	
	scale_colour_manual(values = coefp_colors, guide=FALSE) +
	scale_x_discrete('', labels=TeX(rev(cleanVars))) +	
	ylab(TeX('$\\beta_{p} \\times \\frac{\\sigma_{x_{p}}}{\\sigma_{y}}$')) +
	coord_flip() + 
	theme(
		# legend.position='top', legend.title=element_blank(),
		legend.position='none', legend.title=element_blank(),		
		legend.text=element_text(family="Source Sans Pro Light"),
		panel.border=element_blank(),
		axis.ticks=element_blank(),
		axis.text.x=element_text(family="Source Sans Pro Light"),
		axis.text.y=element_text(family="Source Sans Pro Light", hjust=0)
	)
ggsave(ggCoef, file=paste0(pathGraphics,'betaEst.pdf'), width=7, height=6, device=cairo_pdf)
################