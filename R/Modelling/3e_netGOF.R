################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
source(paste0(fPth, 'gofPlot2.R'))
################

################
# load data
load(paste0(pathResults, 'ameResults.rda')) # load AME mod results
################

################
gof = ameFits$base$GOF
# colnames(gof) = c('rowVar','colVar','dVar','tVar')
varKey = data.frame(dirty=colnames(gof), stringsAsFactors = FALSE)
varKey$clean = c('Sender variation', 'Receiver variation', 
	'Dyadic dependency', 'Triadic dependency')
ggGOF = gofPlot2(gof, FALSE, varKey) +
	theme(
		axis.text.x=element_text(family='Source Sans Pro Light'),
		axis.title.x=element_text(family='Source Sans Pro Light'),
		strip.text.x = element_text(family="Source Sans Pro Semibold")
		)
ggsave(ggGOF, file=paste0(pathGraphics, 'netGOF.pdf'), height=6, width=8, device=cairo_pdf)
################