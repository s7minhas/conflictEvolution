################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }

# load libs
loadPkg(c('igraph', 'ggraph','reshape2'))
################

################
#build random networks
A = matrix( c(0, 0, 0, 1, 2,
	4, 5, 0, 0, 1), 
	nrow=5, ncol=5, byrow = TRUE,
	dimnames=list(LETTERS[1:5], LETTERS[1:5]))
B = matrix( c(0, 0, 0, 1, 2, 
			 4, 0, 0, 0, 1,
			 0, 0, 0, 1, 2,
			 0, 0, 3, 4, 2,
			 0, 0, 2, 4, 3,
			 0, 0, 0, 1, 2),
	nrow=6, ncol=6, byrow = TRUE,
	dimnames=list(LETTERS[1:6], LETTERS[1:6]))
C = matrix(c(0, 0, 0, 1, 2, 
			 4, 5, 0, 0, 1,
			 0, 0, 0, 1, 2,
			 0, 0, 3, 4, 2,
			 0, 0, 3, 4, 3,
			 0, 0, 0, 1, 2,
			 1, 1, 4, 2, 1,
			 4, 5, 6, 1, 3),
	nrow=8, ncol=8, byrow = TRUE,
	dimnames=list(LETTERS[1:8], LETTERS[1:8]))

# convert to longitudinal edgelist
longitNetEL = rbind(
	cbind(reshape2::melt(A), time='t = 1'),
	cbind(reshape2::melt(B), time='t = 2'),
	cbind(reshape2::melt(C), time='t = 3') )
longitNetEL = longitNetEL[longitNetEL$value>0,]
names(longitNetEL)[1:2] = c('from','to')
################

################
# get ready to plot by setting some attribs
g = igraph::graph_from_data_frame(longitNetEL, directed = TRUE)
# V(g)$nSize =  igraph::degree(g, mode='total')

# plot using ggraph
set.seed(6886)
gGG = ggraph(g, layout = 'nicely') + 
    geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) + 
    geom_node_point() + 
    facet_edges(~time) + 
    theme_graph(
    	foreground = "#525252", 
    	fg_text_colour = 'white'
    	) + 
    theme(
    	legend.position = 'none',
    	panel.border=element_blank()
    	)
ggsave(gGG, file=paste0(pathGraphics, 'dummyNet.pdf'), height=6, width=9)
################