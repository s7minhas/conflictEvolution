################
# helper fns for Descriptives/1_netPlots.R
# pull out actor names from list
getActor = function(list){ sort(unique(unlist(lapply(list, rownames))))  }

# get cleaned mat
getNameKey = function(yList){
	vNameKey = data.frame(dirty=getActor(yList), clean=getActor(yList), stringsAsFactors = FALSE)
	vNameKey$clean = gsub(' (Nigeria)', '',vNameKey$clean, fixed=TRUE)
	vNameKey$clean = gsub(' Ethnic', '',vNameKey$clean)
	vNameKey$clean = gsub(' Militia', '\nMilitia',vNameKey$clean)
	vNameKey$clean = trim(vNameKey$clean)
	vNameKey$clean[vNameKey$clean=='MASSOB: Movement for the Actualization of a Sovereign State of Biafra'] = 'MASSOB'
	vNameKey$clean[vNameKey$clean=='MEND: Movement for the Emancipation of the Niger Delta'] = 'MEND'
	vNameKey$clean[vNameKey$clean=='OPC: Oodua Peoples Congress'] = 'OPC'
	vNameKey$clean[vNameKey$clean=='Military Forces of Nigeria'] = 'Military\n(Nigeria)'
	vNameKey$clean[vNameKey$clean=='Police Forces of Nigeria'] = 'Police\n(Nigeria)'
	vNameKey$clean[vNameKey$clean=='Kutep Communal\nMilitia'] = 'Kutep\nMilitia'
	vNameKey$clean[vNameKey$clean=='Shiite Muslim\nMilitia'] = 'Shiite\nMilitia'
	vNameKey$clean[vNameKey$clean=='Sunni Muslim\nMilitia'] = 'Sunni\nMilitia'
	vNameKey$clean[vNameKey$clean=='Bakassi Boys\nMilitia'] = 'Bakassi\nMilitia'
	vNameKey$clean[vNameKey$clean=='Area Boys\nMilitia'] = 'Area\nBoys\nMilitia'
	vNameKey$clean[vNameKey$clean=='Kalo-Kato\nMilitia'] = 'Kalo\nKato\nMilitia'	
	vNameKey$clean[vNameKey$clean=='Boko Haram'] = 'Boko\nHaram'
	return(vNameKey)
}

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