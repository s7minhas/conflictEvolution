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

# adj mats
A[A>0] =1 ; B[B>0] =1 ; C[C>0] =1 
diag(A) = diag(B) = diag(C) = NA
adjMatL = list(A, B, C)
################

################
# set layout positions of nodes
	# 'layout_as_bipartite', 'layout_as_star', 'layout_as_tree',
	# 'layout_in_circle', 'layout_on_grid', 'layout_on_sphere',
	# 'layout_randomly', 'layout_with_dh', 'layout_with_fr',
	# 'layout_with_gem', 'layout_with_graphopt', 'layout_with_kk',
	# 'layout_with_lgl', 'layout_with_mds', 'layout_with_sugiyama',
	# 'layout_nicely'

# start by creating summary array
matchDim = function(x, y){
	orig = rownames(x) ; missRows = setdiff(rownames(y), orig)
	for(i in 1:length(missRows)){ x = rbind(x, 0) ; x = cbind(x, 0) }
	colnames(x) = rownames(x) = c(orig, missRows)
	return(x) }
arr = matchDim(A, C) + matchDim(B, C) + C
gArr = graph_from_adjacency_matrix(arr, mode='directed', weighted=NULL, diag=FALSE)
set.seed(6886)
lArr = layout_with_fr(gArr) ; rownames(lArr) = rownames(arr)
################

################
# get ready to plot by setting some attribs
gL = lapply(1:length(adjMatL), function(ii){
	# convert adjmat to graph
	adjMat = adjMatL[[ii]]
	g = graph_from_adjacency_matrix(adjMat, mode='undirected', weighted=NULL, diag=FALSE)

	# define attributes
	V(g)$nSize = degree(g, mode='total')

	# color by nodes that are new in that t
	V(g)$nColor = rep('gray40', length(V(g)))
	if(ii>1){
		prevNodes = rownames(adjMatL[[ii-1]])
		tNodes = rownames(adjMatL[[ii]])
		newNodes = setdiff(tNodes, prevNodes)
		V(g)$nColor[match(newNodes, names(V(g)))] = 'gray80'
	}

	# plot
	fName = paste0(pathGraphics, 'netPanel_',ii,'.pdf')
	pdf(file=fName)
	plot(g, 
		layout=lArr[names(V(g)),],
		vertex.size=V(g)$nSize*3,
		vertex.frame.color='black',
		vertex.color=V(g)$nColor,
		vertex.label.color=V(g)$nColor,
		vertex.label.cex=.1,
		# edge.arrow.size=.3,
		# edge.width=E(g)$weight/3,
		# edge.curved=TRUE,
		# main=paste0('t=',ii),
		asp=TRUE
		)
	dev.off()
	system(paste('pdfcrop ', fName, fName))
})
################

################
# get ready to plot by setting some attribs
gL = lapply(1:length(adjMatL), function(ii){
	# convert adjmat to graph
	adjMat = adjMatL[[ii]]
	g = graph_from_adjacency_matrix(adjMat, mode='directed', weighted=NULL, diag=FALSE)

	# define attributes
	V(g)$nSize = degree(g, mode='total')

	# color by nodes that are new in that t
	V(g)$nColor = rep('gray40', length(V(g)))
	if(ii>1){
		prevNodes = rownames(adjMatL[[ii-1]])
		tNodes = rownames(adjMatL[[ii]])
		newNodes = setdiff(tNodes, prevNodes)
		V(g)$nColor[match(newNodes, names(V(g)))] = 'gray80'
	}

	# add shapes to distinguish community
	V(g)$nShape = rep('circle', length(V(g)))
	V(g)$nShape[match(c('E','D','A','F'), names(V(g)))] = 'square'

	# plot
	fName = paste0(pathGraphics, 'netPanel2_',ii,'.pdf')
	pdf(file=fName)
	plot(g, 
		layout=lArr[names(V(g)),],
		vertex.size=V(g)$nSize*3,
		vertex.frame.color='black',
		vertex.color=V(g)$nColor,
		vertex.label.color=V(g)$nColor,
		vertex.label.cex=.1,
		vertex.shape=V(g)$nShape,
		edge.arrow.size=.7,
		# edge.width=E(g)$weight/3,
		# edge.curved=TRUE,
		# main=paste0('t=',ii),
		asp=TRUE
		)
	dev.off()
	system(paste('pdfcrop ', fName, fName))
})
################