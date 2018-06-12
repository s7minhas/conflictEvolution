################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
source(paste0(fPth, 'getScenDiff.R'))
################

################
# load data
load(paste0(pathResults, 'ameResults.rda')) # load AME mod results
load(paste0(pathResults, 'glmResults.rda')) # load GLM mod results
################

# plot of marg effs ###########################################
# get data and add intercept val
modData = gfitFullSpec$model ; modData$intercept = 1

# create scenario matrix
vars = colnames(ameFits$base$BETA)
varsMod = gsub('.dyad','',vars,fixed=TRUE)
medVals = apply(modData[,varsMod], 2, mean) ; names(medVals) = varsMod
replaceVal = function(var, newVal, oVals=medVals){ oVals[var] = newVal ; return(oVals) }
getQ = function(x,p,data=modData){ quantile(data[,x], probs=p, na.rm=TRUE) }
scen = cbind(
	postBoko_1=replaceVal('postBoko', 1), 
	postBoko_0=replaceVal('postBoko', 0),
	vioCivRow_1=replaceVal('vioCivEvents.row', 1),	
	vioCivRow_0=replaceVal('vioCivEvents.row', 0),	
	vioCivCol_1=replaceVal('vioCivEvents.col', 1),	
	vioCivCol_0=replaceVal('vioCivEvents.col', 0)	
	)
# split into hi and lo
scen1 = scen[,seq(1,ncol(scen),2)] ; scen0 = scen[,seq(2,ncol(scen),2)]
scens = c( 'Post-Boko Haram', 'Civilian Attacks (Sender)', 'Civilian Attacks (Receiver)')

# for ame
glmDraws = rmvnorm(1000, coef(gfitFullSpec), vcov(gfitFullSpec)) ; colnames(glmDraws)[1] = 'intercept'
ameDraws = rmvnorm(1000, apply(ameFits$base$BETA, 2, median), cov(ameFits$base$BETA))
scenDiffs = rbind(
	getScenDiff(linkType='logit', scen1[colnames(glmDraws),], scen0[colnames(glmDraws),], scens, glmDraws, 'GLM', type='densityShade'),
	getScenDiff(linkType='probit', scen1, scen0, scens, ameFits$base$BETA, 'AME', type='densityShade') )

ggCols = c(GLM='#d6604d', AME='#4393c3')
ggLty = c(GLM='dashed', AME='solid')
scenDiffsSlice = scenDiffs
scenGG = ggplot(data=scenDiffsSlice, aes(color=mod, fill=mod)) +
	geom_line(data=scenDiffsSlice, aes(x=x,y=y)) +
	geom_ribbon(data=subset(scenDiffsSlice,q95), aes(x=x,ymax=y,fill=mod),ymin=0,alpha=0.2) +
	geom_ribbon(data=subset(scenDiffsSlice,q90), aes(x=x,ymax=y,fill=mod),ymin=0,alpha=0.6) +
	geom_vline(aes(xintercept=mean, color=mod,linetype=mod),size=1.2) +	
	scale_color_manual(values=ggCols) + scale_fill_manual(values=ggCols) +
	scale_linetype_manual(values=ggLty) +
	guides(
		linetype=guide_legend(override.aes = list(size=.5)),
		fill=guide_legend(override.aes = list(fill='transparent'))
		) +
	xlab('Pr(Conflict=1 | X=1) - Pr(Conflict=1 | X=0)') +
	ylab('Density') +
	facet_wrap(~scen, scales='free', ncol=1) +
	theme(
		legend.position = 'top', legend.title=element_blank(),
		axis.ticks=element_blank(), axis.text.y=element_blank(),
		panel.border=element_blank(),
		strip.text.x = element_text(size = 9, color='white' ),
		strip.background = element_rect(fill = "#525252", color='#525252')		
		)
scenGG
# ggsave(scenGG, file=xxxx, width=7, height=3)
############################################