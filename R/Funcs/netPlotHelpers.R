################
# helper fns for network related plots

# sum over  array to matrix
getMatfromArr = function(arr, pds=rep(TRUE,dim(arr)[3]), isolRemove=TRUE
	){
	# sum over
	arrSum = apply(arr[,,pds], c(1,2), sum, na.rm=TRUE)
	diag(arrSum) = 0
	# remove isolates
	if(isolRemove){
		toKeep = rownames(arrSum)[which(rowSums(arrSum) + colSums(arrSum)!=0)]
		arrSum=arrSum[toKeep,toKeep] }
	return(arrSum)
}

# conv mat to graph object with attribs and node pos
loadPkg(c('qgraph', 'RColorBrewer'))
getGraphfromMat = function(
	mat, mode='directed', diag=FALSE, weighted=TRUE, 
	colBrksByDegree=seq(0,50,5),
	nodePos=NULL, areaMult=100, repulseExp=3.1, 
	seed=6886
	){
	# create graph object
	g = graph.adjacency(mat, mode=mode, diag=diag, weighted=weighted)

	# add attribs
	g$labPrint = names(V(g))
	g$labSize = rescale(degree(g), c(.4, .7))
	g$vSize = rescale(degree(g), c(10, 16))
	g$vShape = rep('circle', length(V(g)))
	g$vShape[match(c('Military\n(Nigeria)','Police\n(Nigeria)'), names(V(g)))] = 'square'

	# add cols for degree
	pal=colorRampPalette(c(brewer.pal(9,'Blues')[1],brewer.pal(9,'Blues')[9]))(length(colBrksByDegree))
	degBkts = as.numeric(cut(degree(g), colBrksByDegree ))
	V(g)$vCol=pal[degBkts]
	V(g)$vLabCol=rep('black', length(V(g))) ; V(g)$vLabCol[degBkts>3] = 'white'

	# determine node pos
	if(is.null(nodePos)){
		set.seed(seed)
		nodePos = qgraph.layout.fruchtermanreingold(
			edgelist=get.edgelist(g,names=FALSE), vcount=vcount(g),
			area=areaMult*(vcount(g)^2),repulse.rad=(vcount(g)^repulseExp) )
		rownames(nodePos) = rownames(mat)
	} else {
		nodePos = nodePos[names(V(g)),]
	}
	return(list(g=g,nodePos=nodePos))
}

# plot graph
plotGraph = function(
	g, gPos, 
	arrowSize=.65,
	eWidth=(E(g)$weight)^.5,
	vFrameCol='black',
	vLabCol=V(g)$vLabCol,	
	vLabCex=g$labSize,
	vShape=g$vShape,
	vCol=V(g)$col,
	eCurve=TRUE,
	aspLogic=FALSE,
	...
	){
	plot.igraph(
		g, 
		layout=gPos,
		vertex.label=g$labPrint,
		vertex.label.color=vLabCol,
		vertex.label.cex=vLabCex,
		vertex.frame.color=vFrameCol,	
		vertex.color=vCol,
		# vertex.color='white',
		# vertex.size=g$vSize, 
		# could fix see stackoverflow, eqarrowPlot, for start
		vertex.shape=vShape,
		edge.arrow.size=arrowSize,
		edge.width=eWidth,
		edge.curved=eCurve,
		asp = aspLogic, 
		...
		)	
}
################