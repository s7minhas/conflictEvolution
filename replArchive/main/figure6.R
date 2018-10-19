################
# workspace
source('setup.R')
source('postHelpers.R')
source('actorInfo.R')
loadPkg('gridExtra')
################

################
# load data
load('ameResults.rda') # load AME mod results
load('nigeriaMatList_acled_v7.rda') # loads yList object
yrs = char(2000:2016) ; yList = yList[yrs]
################

################
sendData = getAddEffData(ameFits$base, row=TRUE, yList=yList)
sendData$yLabel='Sender Effects ($a_{i}$)'
recData = getAddEffData(ameFits$base, row=FALSE, yList=yList)
recData$yLabel='Receiver Effects ($b_{j}$)'

# stdz row
sr = sendData[,c('actor','addEff')]
sr$addEffRec = recData$addEff[match(sr$actor,recData$actor)]
sr$effMu = (sr$addEff + sr$addEffRec)/2
aOrder = rev(char(sr$actor[order(sr$effMu,decreasing = TRUE)]))

sendData$actor = factor(sendData$actor, levels=aOrder)
recData$actor = factor(recData$actor, levels=aOrder)
srData = rbind(sendData, recData)
srData$yLabel = factor(srData$yLabel, levels=unique(srData$yLabel))

facet_labeller = function(string){ TeX(string) }
srPlot = addEffPlot(addEffData=srData,row=FALSE) +
	coord_flip() + xlab('') + ylab('') +
	facet_wrap(~yLabel, labeller=as_labeller(facet_labeller, default = label_parsed)) +
	theme(
		axis.text.x=element_text(angle=0),
		strip.text.x = element_text(size = 10, color='white',
			angle=0, hjust=.03),
		strip.background = element_rect(fill = "#525252", color='#525252')
		)	
ggsave(srPlot, 
	file='floats/figure6.pdf', 
	width=7, height=6)
################