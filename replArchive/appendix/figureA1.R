################
# workspace
source('../main/setup.R')
source('../main/paramPlot2.R')
################

################
# load data
load('../main/ameResults.rda') # load AME mod results

# quick trace plot
mcmcData = ameFits$base$BETA
varKey = data.frame(dirty=colnames(mcmcData),stringsAsFactors=FALSE)
varKey$clean = c(
	'Intercept',
	'Riots/Protests Against (Sender)', 'Civilian Attacks (Sender)', 'Geographic Spread (Sender)',
	'Riots/Protests Against (Receiver)', 'Civilian Attacks (Receiver)', 'Geographic Spread (Receiver)',
	'Gov-Gov Actors','Post-Boko Haram', 'Election Year', 'Neighborhood Conflict')
varKey = varKey[c(9,2,5,3,6,4,7,10,11,8,1),]
ggsave(
	paramPlot2(mcmcData, varKey), 
	file='floats/figureA1.pdf', width=8,height=9)
################