################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }

# load fns from helper file
source(paste0(fPth, 'netPlotHelpers.R'))
source(paste0(fPth, 'actorInfo.R'))
################

################
# load data
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
yrs = char(2000:2016) ; yList = yList[yrs]
################

################
# clean up row/col names in yList
vNameKey = getNameKey(yList)
yList = lapply(yList, function(y){
	rownames(y) = vNameKey$clean[match(rownames(y),vNameKey$dirty)]
	colnames(y) = vNameKey$clean[match(colnames(y),vNameKey$dirty)]
	return(y) })
################

################
# set up actor positions across T
yArr = listToArray(actors=getActor(yList), Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
yArrSum = getMatfromArr(arr=yArr)
govActors = c('Military\n(Nigeria)','Police\n(Nigeria)')

# pre post boko haram arrays
bhIDs = unlist(lapply(yList, function(x){'Boko\nHaram' %in% rownames(x)}))
yArrSumPreBH = getMatfromArr(arr=yArr, pds=!bhIDs)
yArrSumPostBH = getMatfromArr(arr=yArr, pds=bhIDs)

# set up pos mat
tmp=getGraphfromMat(mat=yArrSum) ; gArrSum=tmp$g ; gArrPos=tmp$nodePos ; rm(tmp)
tmp=getGraphfromMat(mat=yArrSumPreBH, nodePos=gArrPos)
gArrSumPreBH=tmp$g ; gArrPosPreBH=tmp$nodePos ; rm(tmp)
tmp=getGraphfromMat(mat=yArrSumPostBH, nodePos=gArrPos)
gArrSumPostBH=tmp$g ; gArrPosPostBH=tmp$nodePos ; rm(tmp)
################

################
# plot by full, pre and post bh periods
fName = paste0(pathGraphics, 'nigeria_2000_2016.pdf') ; pdf(file=fName, width=13,height=9)
vCol = ifelse(names(V(gArrSum)) %in% govActors, 'gray30', 'gray95')
vLabCol = ifelse(names(V(gArrSum)) %in% govActors, 'white', 'gray30')
plotGraph(gArrSum, gArrPos, 
	vShape='circle', vLabCex=gArrSum$labSize+.3, 
	vLabCol=vLabCol, vertex.label.font=1, vertex.size=gArrSum$vSize, 
	vFrameCol=vCol, vCol=vCol, vertex.label.family="Helvetica")
title('Nigerian Intra-State Conflict\n(2000-2016)', family='Helvetica', adj=1, line=-4)	
dev.off() ; system(paste('pdfcrop',fName,fName, sep=' '))
################

################
fName = paste0(pathGraphics, 'nigeria_preBK.pdf') ; pdf(file=fName, width=13,height=9)
vCol = ifelse(names(V(gArrSumPreBH)) %in% govActors, 'gray30', 'gray95')
vLabCol = ifelse(names(V(gArrSumPreBH)) %in% govActors, 'white', 'gray30')
plotGraph(gArrSumPreBH, gArrPosPreBH, 
	vShape='circle', vLabCex=gArrSumPreBH$labSize+.3, 
	vLabCol=vLabCol, vertex.label.font=1, vertex.size=gArrSumPreBH$vSize, 
	vFrameCol=vCol, vCol=vCol, vertex.label.family="Helvetica")
title('Nigerian Intra-State Conflict Pre-Boko Haram\n(2000-2008)', family='Helvetica', adj=1, line=-4)	
dev.off() ; system(paste('pdfcrop',fName,fName, sep=' '))
################

################
# plot post BH interaction
# define edge colors by pre post boko interactions
edgePreBH = data.frame( edge=attributes(E(gArrSumPreBH))$vnames,
	edgeWgt=E(gArrSumPreBH)$weight, stringsAsFactors = FALSE )
edgePostBH = data.frame( edge=attributes(E(gArrSumPostBH))$vnames,
	edgeWgt=E(gArrSumPostBH)$weight, stringsAsFactors = FALSE )
edgePostBH$wgtPreBH = edgePreBH$edgeWgt[match(edgePostBH$edge,edgePreBH$edge)]
edgePostBH$wgtPreBH[is.na(edgePostBH$wgtPreBH)] = 0
edgePal = brewer.pal(7,'Paired')[c(5,1,3)]
edgePostBH$edgeCol = ifelse(edgePostBH$edgeWgt>edgePostBH$wgtPreBH, edgePal[2],edgePal[1])
edgePostBH$edgeCol[edgePostBH$edgeWgt==edgePostBH$wgtPreBH]=edgePal[3]

# define v cols by actor type
vCol = ifelse(names(V(gArrSumPostBH)) %in% govActors, 'gray30', 'gray95')
vLabCol = ifelse(names(V(gArrSumPostBH)) %in% govActors, 'white', 'gray30')
fName = paste0(pathGraphics, 'nigeria_postBK.pdf') ; pdf(file=fName, width=13,height=9)
plotGraph(gArrSumPostBH, gArrPosPostBH, 
	edge.color=edgePostBH$edgeCol, vShape='circle', vLabCex=gArrSumPostBH$labSize+.3, 
	vLabCol=vLabCol, vertex.label.font=1, vertex.size=gArrSumPostBH$vSize, 
	vFrameCol=vCol, vCol=vCol, vertex.label.family="Helvetica")
title('Nigerian Intra-State Conflict Post-Boko Haram\n(2009-2016)', family='Helvetica', adj=1, line=-4)
dev.off() ; system(paste('pdfcrop',fName,fName, sep=' '))
################

################
# plot by t
selT = rep(FALSE, dim(yArr)[3])
lapply(1:length(yList), function(t){
	selT[t] = TRUE
	tmp = getGraphfromMat(
		mat=getMatfromArr(yArr, pds=selT), nodePos=gArrPos,
		colBrksByDegree=seq(0,12,3),
		)
	g = tmp$g ; pos = tmp$nodePos ; rm(tmp)
	vCol = ifelse(names(V(g)) %in% govActors, 'gray30', 'gray95')
	vLabCol = ifelse(names(V(g)) %in% govActors, 'white', 'gray30')	
	fName = paste0(pathGraphics, 'nigeriaT_', names(yList)[t], '.png')
	png(filename=fName, width=1500,height=900)
	plotGraph(g, pos, 
		eCurve=FALSE, arrowSize=1.5,		
		vShape='circle', vLabCex=g$labSize+.6,
		vertex.size=g$vSize^2.25, 
		vertex.label.font=1, vertex.label.family="Helvetica",
		vLabCol=vLabCol, vFrameCol=vCol, vCol=vCol,
		aspLogic=0, rescale=FALSE,
		ylim=c(min(gArrPos[,2]), max(gArrPos[,2])),
		xlim=c(min(gArrPos[,1]), max(gArrPos[,1]))
		)
	title(names(yList)[t], family='Helvetica', adj=1, line=-2, cex.main=4)
	dev.off()
})
setwd(pathGraphics)
system("convert -delay 75 -loop 0 nigeriaT_*.png nigeriaConf.gif ; rm -rf nigeriaT_*.png")
################