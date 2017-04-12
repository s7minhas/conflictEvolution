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
mcmcData = ameFits$base$BETA
varKey = data.frame(dirty=colnames(mcmcData),stringsAsFactors=FALSE)
varKey$clean = c(
	'Intercept',
	'Riots/Protests Against (Sender)', 'Civilian Attacks (Sender)', 'Geographic Spread (Sender)',
	'Riots/Protests Against (Receiver)', 'Civilian Attacks (Receiver)', 'Geographic Spread (Receiver)',
	'Gov-Gov Actors','Post-Boko Haram', 'Election Year', 'Neighborhood Conflict')
varKey = varKey[c(9,2,5,3,6,4,7,10,11,8,1),]
# ggsave(paramPlot2(mcmcData, varKey), file=paste0(pathGraphics, 'betaTrace.pdf'), width=8,height=9)
################

################
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
ameBETA = ameBETA[-which(ameBETA$var %in% c('intercept','govActor.dyad')),]

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
	'Post-Boko\nHaram Period$_{t}$',
	'Election Year$_{t}$',
	'Neighborhood Conflict$_{t}$'
	)
ameBETA$typeClean = c(
	rep('... Sender-Level Covariates',3),
	rep('... Receiver-Level Covariates',3),
	rep('Parameter Estimates for Country-Level Covariates',3)
	)

# add sig info
ameBETA = getSigVec(ameBETA)
################

################
# viz
cleanVars = ameBETA$varClean
ameBETA$typeClean = factor(ameBETA$typeClean, levels=unique(ameBETA$typeClean)[c(3,1,2)])
ggCoef=ggplot(ameBETA, aes(x=varClean, y=mean, color=sig)) + facet_wrap(~typeClean,ncol=1,scales='free_y') + 
	geom_hline(aes(yintercept=0), linetype=2, color = "black") + 
	geom_point(size=4) + 
	geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1.5) + 
	geom_linerange(aes(ymin=lo95,ymax=hi95),alpha = 1, size = .5) +	
	scale_colour_manual(values = coefp_colors, guide=FALSE) +
	scale_x_discrete('', labels=TeX(rev(cleanVars))) +	
	ylab(TeX('$\\beta_{p} \\times \\frac{\\sigma_{x_{p}}}{\\sigma_{y}}$')) +
	coord_flip() + 
	theme_light(base_family="Source Sans Pro") +
	theme(
		legend.position='none', legend.title=element_blank(),
		panel.border=element_blank(),
		axis.ticks=element_blank(),
		axis.text.x=element_text(family="Source Sans Pro Light"),
		axis.text.y=element_text(family="Source Sans Pro Light", hjust=0),
		strip.text.x = element_text(size = 9, color='white',family="Source Sans Pro Semibold", angle=0, hjust=.95),
		strip.background = element_rect(fill = "#525252", color='#525252')				
	)
ggsave(ggCoef, file=paste0(pathGraphics,'betaEst.pdf'), width=7, height=6, device=cairo_pdf)
################