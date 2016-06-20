#################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }
#################

#################
# Load results
load( paste0(pathResults, 'barResults.rda') )
#################

#################
# Parameter effects
coefData = cbind(theta, ses_theta, theta/ses_theta) %>% data.frame()
names(coefData) = c('est', 'se', 't')
vars = c( 'Conflict$_{ij,t-1}$', 'Conflict$_{ji,t-1}$', 'Conflict$_{ijk,t-1}$', 
	'Protest_$_{i,t-1}$', 'DTO$_{i,t-1}$', 'Betweenness\nCentrality$_{i,t-1}$' ) # make sure order of vars matches rows in coefData
varsLevel = rev( c( 'Conflict$_{ij,t-1}$', 'Conflict$_{ji,t-1}$', 'Conflict$_{ijk,t-1}$', 
	'Protest_$_{i,t-1}$', 'Betweenness\nCentrality$_{i,t-1}$', 'DTO$_{i,t-1}$' ) )
coefData$var = vars
coefData$var = factor(coefData$var, levels=varsLevel)

# Construct coefficient plot
source(paste0(fPth, 'postHelpers.R'))
coefP=buildCoef(coefData)
ggsave(coefP, file=paste0(pathGraphics, 'coefP.pdf'), width=6, height=4)
#################

#################
# AB plot
actors = rownames(A)
colorMat = matrix(cbind(actors, 1, 'black'), ncol=3)
colorMat[1:3,2] = brewer.pal(9,'OrRd')[4]
colorMat[4:nrow(colorMat),2] = brewer.pal(length(4:nrow(colorMat))+2,'Blues')[3:(nrow(colorMat)-1)]

abPlot = function( Infl, pThresh, 
	cAbb = NULL, includeIsolates=TRUE, seed=6886,
	edgeArrowSize=1.2, cntryCols=colorMat[,2], textCols=colorMat[,3],
	fName, pWidth=12, pHeight=8, save=FALSE ){
	
	# conf int
	relBin = Infl>quantile(c(Infl),1-pThresh/2)*1
	pThreshLab = (1-pThresh)*100

	# narrow to actor network
	vShapes = 'circle'
	if(!is.null(cAbb)){
		subNetActors = c(which(relBin[cAbb,] != 0), which(relBin[,cAbb] != 0)) %>% names() %>% unique() %>% append(cAbb,.)
		relBin = relBin[subNetActors,subNetActors]
		vShapes = rep('circle', nrow(relBin))
		vShapes[match(cAbb, rownames(relBin))] = 'square'
	}

	# throw out isolates
	if(!includeIsolates){
		activeNodes = names( which(rowSums(relBin) + colSums(relBin) > 0) )
		if( length(activeNodes)==0 ){ return(paste0('No countries with interactions at threshold of ', pThreshLab))  }
		relBin = relBin[activeNodes,activeNodes]
		cntryCols = cntryCols[activeNodes]
		textCols = textCols[activeNodes]
	}

	# Other graph params
	set.seed(seed)
	g = graph.adjacency(relBin, mode='directed', diag=FALSE)
	g$labSize = rescale(igraph::degree(g), c(.5, .9))
	g$vSize = rescale(igraph::degree(g), c(6, 12))
	gLayout = layout.circle(g)

	if(save){ pdf(file=fName, width=pWidth, height=pHeight) }
	plot.igraph(g,
			layout=gLayout,
			vertex.label.color=textCols, 
			vertex.color=cntryCols, 
			vertex.label.cex=1.5,
			vertex.label.dist=.9,
			vertex.size=g$vSize,
			vertex.shape=vShapes,
			edge.arrow.size=edgeArrowSize,
			asp=FALSE	
		)
	if(save){ dev.off() }
	if(save){ system(paste('pdfcrop', fName, fName, sep=' ')) }
}

abPlot(Infl=A, pThresh=.33, save=TRUE, fName=paste0(pathGraphics, 'senSpace.pdf'))
abPlot(Infl=B, pThresh=.1, save=TRUE, fName=paste0(pathGraphics, 'recSpace.pdf'))
#################