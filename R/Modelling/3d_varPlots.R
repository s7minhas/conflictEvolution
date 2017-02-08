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
# labs
vcKey = data.frame(dirty=colnames(fitFullSpec$VC)[-ncol(fitFullSpec$VC)], stringsAsFactors = FALSE) 
vcKey$clean = c(
	'Within-Sender\nCovariance\n$\\sigma_{a]^{2}$',
	'Sender-Receiver\nCovariance\n$\\sigma_{ab]$',
	'Within-Receiver\nCovariance\n$\\sigma_{b]^{2}$',
	'Reciprocity\n$\\rho$'
	)
################

################
# org data
vc = t(apply(fitFullSpec$VC[,-ncol(fitFullSpec$VC)], 2, summStats))
colnames(vc) = c('mean','lo95','hi95','lo90','hi90')
vc = data.frame(vc, stringsAsFactors = FALSE)
vc$var = rownames(vc) ; rownames(vc) = NULL
vc$varClean = vcKey$clean[match(vc$var, vcKey$dirty)]
vc$varClean = factor(vc$varClean, levels=rev(vcKey$clean[c(1,3,2,4)]))
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

################
# viz s/r eff

# adjust actor names
source(paste0(fPth, 'netPlotHelpers.R'))
vNameKey = getNameKey(yList)
vNameKey$clean[vNameKey$clean=="Kalo\nKato\nMilitia"]="Kalo\nMilitia"
vNameKey$clean[vNameKey$clean=="Area\nBoys\nMilitia"]="Area\nBoys"
names(fitFullSpec$APM) = vNameKey$clean[match(names(fitFullSpec$APM), vNameKey$dirty)]
names(fitFullSpec$BPM) = vNameKey$clean[match(names(fitFullSpec$BPM), vNameKey$dirty)]
yList = lapply(yList, function(y){
	rownames(y) = colnames(y) = vNameKey$clean[match(rownames(y), vNameKey$dirty)]
	return(y) })

source(paste0(fPth, 'postHelpers.R'))
sendEff=addEffPlot(
	fitFullSpec, 
	row=TRUE, addDegree=FALSE, yList=yList, orderByDegree=FALSE) + 
	theme(
		axis.text.x=element_text(angle=45, size=7, family='Source Sans Pro Light'),
		axis.text.y=element_text(family='Source Sans Pro Light'),
		axis.title.y=element_text(family='Source Sans Pro Light')
		)

recEff=addEffPlot(
	fitFullSpec, 
	row=FALSE, addDegree=FALSE, yList=yList, orderByDegree=FALSE) + 
	theme(
		axis.text.x=element_text(angle=45, size=7, family='Source Sans Pro Light'),
		axis.text.y=element_text(family='Source Sans Pro Light'),
		axis.title.y=element_text(family='Source Sans Pro Light')
		)

ggsave(sendEff, file=paste0(pathGraphics, 'aEst.pdf'), width=12, height=5, device=cairo_pdf)
ggsave(recEff, file=paste0(pathGraphics, 'bEst.pdf'), width=12, height=5, device=cairo_pdf)
################