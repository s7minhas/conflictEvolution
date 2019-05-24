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
srPlot = addEffPlot(addEffData=srData,row=FALSE,bw=TRUE) +
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
	# file='floats/figure6.pdf', 
	file='floats/figure6_bw.pdf', 	
	width=7, height=6, 
	device=cairo_pdf)
################