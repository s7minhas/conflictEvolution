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

################
loadPkg('ggrepel')
ggCirc = function(
	Y, U=NULL, V=NULL, row.names=rownames(Y), col.names=colnames(Y),
	vscale=.8, prange=c(2,5), lcol='gray85', ltype='dotted', lsize=.5,
	showActLinks=TRUE, geomLabel=TRUE, geomText=FALSE, geomPoint=TRUE, ...
	){
	
	if (is.null(U)) {
	    a <- rowMeans(Y, na.rm = TRUE)
	    b <- colMeans(Y, na.rm = TRUE)
	    Y0 <- Y
	    Y0[is.na(Y)] <- (outer(a, b, "+"))[is.na(Y)]
	    Y0 <- Y0 - mean(Y0)
	    if (!all(Y == t(Y), na.rm = TRUE)) {
	        sY <- svd(Y0)
	        u <- sY$u[, 1:2]
	        v <- sY$v[, 1:2]
	        mu <- sqrt(apply(u^2, 1, sum))
	        mv <- sqrt(apply(v^2, 1, sum))
	        u <- diag(1/mu) %*% u
	        v <- diag(1/mv) %*% v * vscale
	    }
	    if (all(Y == t(Y), na.rm = TRUE)) {
	        eY <- eigen(Y0)
	        bv <- which(abs(eY$val) >= sort(abs(eY$val), decreasing = TRUE)[2])[1:2]
	        u <- eY$vec[, bv]
	        mu <- sqrt(apply(u^2, 1, sum))
	        u <- diag(1/mu) %*% u
	        mv <- mu
	        v <- u
	    }
	}
	if (!is.null(U)) {
	    if (is.null(V)) {
	        V <- U
	        vscale <- 1
	    }
	    mu <- sqrt(apply(U^2, 1, sum))
	    mv <- sqrt(apply(V^2, 1, sum))
	    u <- diag(1/mu) %*% U
	    v <- diag(1/mv) %*% V * vscale
	}

	rsum <- apply(abs(Y), 1, sum, na.rm = TRUE)
	csum <- apply(abs(Y), 2, sum, na.rm = TRUE)
	links <- which(Y != 0, arr.ind = TRUE)
	
	# org df for gg
	uG = data.frame(u*1.2)
	uG$actor = rownames(yArrSumm)
	uG$tPch = 0 ; uG$tPch[rsum>0] = (mu[rsum>0])^3
	uG = uG[uG$tPch>0,]
	uG$tPch = uG$tPch
	
	# add v if supplied
	if(!is.null(V)){
		vG = data.frame(v*1.2)
		vG$actor = rownames(yArrSumm)
		vG$tPch = 0 ; vG$tPch[csum>0] = (mv[csum>0])^3
		vG = vG[vG$tPch>0,]
		vG$tPch = vG$tPch
		
		uG$eff = 'u' ; vG$eff = 'v'
		uG = rbind(uG, vG)		
		ggCirc = ggplot(uG, aes(x=X1, y=X2,color=eff))
	}
	if(is.null(V)){
		ggCirc = ggplot(uG, aes(x=X1, y=X2))
	}
	
	# add segments
	if(showActLinks){
		for(i in 1:nrow(links)){
			ggCirc = ggCirc + geom_segment(
				x=u[links[i,1],1]*1.2, y=u[links[i,1],2]*1.2,
				xend=v[links[i,2],1]*1.2, yend=v[links[i,2],2]*1.2,
				color=lcol, linetype=ltype, size=lsize ) }
	}
	if(geomPoint){ ggCirc = ggCirc + geom_point() }
	if(geomLabel){ ggCirc = ggCirc + geom_label_repel(aes(label=actor, size=tPch, ...)) }
	if(geomText){ ggCirc = ggCirc + geom_text_repel(aes(label=actor, size=tPch, ...)) }
	ggCirc = ggCirc + scale_size(range=prange) +
		theme(
			legend.position='none',
			axis.ticks=element_blank(),
			axis.title=element_blank(),
			axis.text=element_blank(),
			panel.border=element_blank(),
			panel.grid=element_blank()
			)
	return(ggCirc)
}
################