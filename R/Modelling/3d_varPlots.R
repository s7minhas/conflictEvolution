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
################

################
# org data
vc = t(apply(fitFullSpec$VC[,-ncol(fitFullSpec$VC)], 2, summStats))
colnames(vc) = c('mean','lo95','hi95','lo90','hi90')
vc = data.frame(vc, stringsAsFactors = FALSE)
vc$var = rownames(vc) ; rownames(vc) = NULL
vc$varClean = c('$\\sigma_{a]$', '$\\sigma_{ab]$', '$\\sigma_{b]$', '$\\rho$')
vc$varClean = factor(vc$varClean, 
	levels=rev(c( '$\\sigma_{a]$', '$\\sigma_{b]$', '$\\sigma_{ab]$', '$\\rho$' )))
################

################
# plot
ggVC = ggplot(vc, aes(x=varClean, y=mean)) + 
	geom_point(size=2.5) + 
	geom_linerange(aes(ymin=lo95,ymax=hi95), linetype=1, size=.5) + 
	geom_linerange(aes(ymin=lo90,ymax=hi90), linetype=1, size=1.5) + 
	scale_x_discrete('',labels=TeX(levels(rev(vc$varClean)))) + ylab('') +
	coord_flip() + 
	theme(
		axis.ticks=element_blank(),
		panel.border=element_blank(),
		axis.text=element_text(family='Source Sans Pro Light')
		)
ggsave(ggVC, file=paste0(pathGraphics,'vcEst.pdf'), width=5, height=3, device=cairo_pdf)
################