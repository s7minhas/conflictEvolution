################
# workspace
source('../main/setup.R')
source('../main/gofPlot2.R')
################

################
# load data
load('../main/ameResults.rda') # load AME mod results
################

################
gof = ameFits$base$GOF
varKey = data.frame(dirty=colnames(gof), stringsAsFactors = FALSE)
varKey$clean = c('Sender variation', 'Receiver variation', 
	'Dyadic dependency', 'Triadic dependency')
ggGOF = gofPlot2(gof, FALSE, varKey)
ggsave(ggGOF, file='floats/figureA7.pdf', height=6, width=8)
################