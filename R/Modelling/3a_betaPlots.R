################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
################

################
# load data
load(paste0(pathResults, 'ameResults.rda')) # load AME mod results
load(paste0(pathResults, 'glmResults.rda')) # load GLM mod results
yArr = listToArray(actors=sort(unique(unlist(lapply(yList,rownames)))), 
	Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
################

################
# calc standardized reg coefs
sdY = sd(gfitFullSpec$data$value)
xVars = colnames(fitFullSpec$BETA) %>% gsub('.dyad','',.) %>% .[-1]
sdX = apply(gfitFullSpec$data[,xVars], 2, sd)

#
tmp=fitFullSpec$BETA[,2:ncol(fitFullSpec$BETA)]
for(i in 1:ncol(tmp)){ tmp[,i] = tmp[,i] * sdX[i]}
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
ameBETA$sig = NA
ameBETA$sig[ameBETA$lo90 > 0 & ameBETA$lo95 < 0] = "Positive at 90"
ameBETA$sig[ameBETA$lo95 > 0] = "Positive"
ameBETA$sig[ameBETA$hi90 < 0 & ameBETA$hi95 > 0] = "Negative at 90"
ameBETA$sig[ameBETA$hi95 < 0] = "Negative"
ameBETA$sig[ameBETA$lo90 < 0 & ameBETA$hi90 > 0] = "Insignificant"
coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255),
	"Negative"= rgb(222, 45, 38, maxColorValue=255),
	"Positive at 90"=rgb(158, 202, 225, maxColorValue=255),
	"Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
	"Insignificant" = rgb(150, 150, 150, maxColorValue=255))
################

################
# plot
ggCoef=ggplot(ameBETA, aes(x=varClean, y=mean, color=sig)) + 
	geom_hline(aes(yintercept=0), linetype=2, color = "black") + 
	geom_point(size=2.5) + 
	geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1.5) + 
	geom_linerange(aes(ymin=lo95,ymax=hi95),alpha = 1, size = .5) +	
	scale_colour_manual(values = coefp_colors, guide=FALSE) +
	scale_x_discrete('', labels=TeX(rev(cleanVars))) +	
	ylab(TeX('$\\beta_{p} \\times \\frac{\\sigma_{x_{p}}}{\\sigma_{y}}$')) +
	coord_flip() + 
	theme(
		legend.position='none',
		panel.border=element_blank(),
		axis.ticks=element_blank(),
		axis.text.x=element_text(family="Source Sans Pro Light"),
		axis.text.y=element_text(family="Source Sans Pro Light", hjust=0)
	)
ggsave(ggCoef, file=paste0(pathGraphics,'betaEst.pdf'), width=7, height=5, device=cairo_pdf)
################