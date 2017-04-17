################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
source(paste0(fPth, 'postHelpers.R'))
source(paste0(fPth, 'actorInfo.R'))
loadPkg('gridExtra')
################

################
# load data
load(paste0(pathResults, 'ameResults.rda')) # load AME mod results
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
yrs = char(2000:2016) ; yList = yList[yrs]
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
	# coord_flip() + 
	theme(
		legend.position = 'none',
		axis.ticks=element_blank(),
		panel.border=element_blank(),
		axis.text.x=element_text(vjust=-1, family='Source Sans Pro Light'),
		axis.text.y=element_text(family='Source Sans Pro Light'),
		strip.text.x = element_text(size = 10, color='white',
			family="Source Sans Pro Semibold", angle=0, hjust=.03),
		strip.background = element_rect(fill = "#525252", color='#525252')
		)
ggsave(ggVC, file=paste0(pathGraphics,'vcEst.pdf'), width=8, height=2, device=cairo_pdf)
################

################
# viz s/r eff

# adjust actor names
vNameKey = getNameKey(yList)
vNameKey$clean[vNameKey$clean=="Kalo\nKato\nMilitia"]="Kalo\nMilitia"
vNameKey$clean[vNameKey$clean=="Area\nBoys\nMilitia"]="Area\nBoys"
vNameKey$clean = gsub('\n',' ', vNameKey$clean, fixed=TRUE)
names(ameFits$base$APM) = vNameKey$clean[match(names(ameFits$base$APM), vNameKey$dirty)]
names(ameFits$base$BPM) = vNameKey$clean[match(names(ameFits$base$BPM), vNameKey$dirty)]
yList = lapply(yList, function(y){
	rownames(y) = colnames(y) = vNameKey$clean[match(rownames(y), vNameKey$dirty)]
	return(y) })

sendEff=addEffPlot(
	ameFits$base, 
	row=TRUE, addDegree=FALSE, yList=yList, orderByDegree=FALSE) + 
	coord_flip() + 
	theme(
		axis.text.x=element_text(angle=80, size=9, family='Source Sans Pro Light'),
		axis.text.y=element_text(family='Source Sans Pro Light'),
		axis.title.y=element_text(family='Source Sans Pro Light')
		)

recEff=addEffPlot(
	ameFits$base, 
	row=FALSE, addDegree=FALSE, yList=yList, orderByDegree=FALSE) + 
	theme(
		axis.text.x=element_text(angle=80, size=9, family='Source Sans Pro Light'),
		axis.text.y=element_text(family='Source Sans Pro Light'),
		axis.title.y=element_text(family='Source Sans Pro Light')
		)

ggsave(sendEff, file=paste0(pathGraphics, 'aEst.pdf'), width=9, height=4, device=cairo_pdf)
ggsave(recEff, file=paste0(pathGraphics, 'bEst.pdf'), width=9, height=4, device=cairo_pdf)
################	

################
sendData = getAddEffData(ameFits$base, row=TRUE, yList=yList)
sendData$yLabel='Sender Effects ($a_{i}$)'
recData = getAddEffData(ameFits$base, row=FALSE, yList=yList)
recData$yLabel='Receiver Effects ($b_{j}$)'

facet_labeller = function(string){ TeX(string) }

sPlot = addEffPlot(addEffData=sendData,row=TRUE) +
	coord_flip() + xlab('') + ylab('') +
	facet_wrap(~yLabel, labeller=as_labeller(facet_labeller, default = label_parsed)) + 
	theme(
		axis.text.x=element_text(family='Source Sans Pro Light',angle=0),
		axis.text.y=element_text(family='Source Sans Pro Light'),
		strip.text.x = element_text(size = 10, color='white',
			family="Source Sans Pro Semibold", angle=0, hjust=.03),
		strip.background = element_rect(fill = "#525252", color='#525252')
		)
rPlot = addEffPlot(addEffData=recData,row=FALSE) +
	coord_flip() + xlab('') + ylab('') +
	facet_wrap(~yLabel, labeller=as_labeller(facet_labeller, default = label_parsed)) + 
	theme(
		axis.text.x=element_text(family='Source Sans Pro Light',angle=0),
		axis.text.y=element_text(family='Source Sans Pro Light'),
		strip.text.x = element_text(size = 10, color='white',
			family="Source Sans Pro Semibold", angle=0, hjust=.03),
		strip.background = element_rect(fill = "#525252", color='#525252')
		)	
ggsave(
	grid.arrange(sPlot, rPlot, ncol=2),
	file=paste0(pathGraphics, 'abEst.pdf'), device=cairo_pdf)

# stdz row
sr = sendData[,c('actor','addEff')]
sr$addEffRec = recData$addEff[match(sr$actor,recData$actor)]
sr$effMu = (sr$addEff + sr$addEffRec)/2
aOrder = rev(char(sr$actor[order(sr$effMu,decreasing = TRUE)]))

sendData$actor = factor(sendData$actor, levels=aOrder)
recData$actor = factor(recData$actor, levels=aOrder)
srData = rbind(sendData, recData)
srData$yLabel = factor(srData$yLabel, levels=unique(srData$yLabel))

srPlot = addEffPlot(addEffData=srData,row=FALSE) +
	coord_flip() + xlab('') + ylab('') +
	facet_wrap(~yLabel, labeller=as_labeller(facet_labeller, default = label_parsed)) +
	theme(
		axis.text.x=element_text(family='Source Sans Pro Light',angle=0),
		axis.text.y=element_text(family='Source Sans Pro Light'),
		strip.text.x = element_text(size = 10, color='white',
			family="Source Sans Pro Semibold", angle=0, hjust=.03),
		strip.background = element_rect(fill = "#525252", color='#525252')
		)	
ggsave(srPlot, 
	file=paste0(pathGraphics, 'abEst2.pdf'), 
	width=7, height=6, device=cairo_pdf)
################