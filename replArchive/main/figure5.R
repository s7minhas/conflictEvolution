################
# workspace
source('setup.R')

# load fns from helper file
source('netPlotHelpers.R')
source('actorInfo.R')
################

################
# load data
load('nigeriaMatList_acled_v7.rda') # loads yList object
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
yArr = listToArray(
	actors=getActor(yList), Y=yList, 
	Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
yArrSum = getMatfromArr(arr=yArr)
govActors = c('Military\n(Nigeria)','Police\n(Nigeria)')

# pre post boko haram arrays
bhIDs = unlist(lapply(yList, function(x){'Boko\nHaram' %in% rownames(x)}))
yArrSumPreBH = getMatfromArr(arr=yArr, pds=!bhIDs)
yArrSumPostBH = getMatfromArr(arr=yArr, pds=bhIDs)

# set up pos mat
tmp=getGraphfromMat(mat=yArrSum)
gArrSum=tmp$g ; gArrPos=tmp$nodePos
tmp=getGraphfromMat(mat=yArrSumPreBH, nodePos=gArrPos)
gArrSumPreBH=tmp$g ; gArrPosPreBH=tmp$nodePos
tmp=getGraphfromMat(mat=yArrSumPostBH, nodePos=gArrPos)
gArrSumPostBH=tmp$g ; gArrPosPostBH=tmp$nodePos
rm(tmp)
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
edgePal = brewer.pal(3,'Set1')
edgePostBH$edgeCol = ifelse(
	edgePostBH$edgeWgt>edgePostBH$wgtPreBH, edgePal[2],edgePal[1])
edgePostBH$edgeCol[edgePostBH$edgeWgt==edgePostBH$wgtPreBH]=edgePal[3]
E(gArrSumPostBH)$weight = E(gArrSumPostBH)$weight*3

# define v cols by actor type
vCol = ifelse(names(V(gArrSumPostBH)) %in% govActors, 'gray30', 'gray95')
vLabCol = ifelse(names(V(gArrSumPostBH)) %in% govActors, 'white', 'gray30')
fName = 'floats/figure5.pdf'
pdf(file=fName, width=13,height=9)
plotGraph(gArrSumPostBH, gArrPosPostBH, 
	edge.color=edgePostBH$edgeCol, vShape='circle', vLabCex=gArrSumPostBH$labSize+.3, 
	vLabCol=vLabCol, vertex.label.font=1, vertex.size=gArrSumPostBH$vSize, 
	vFrameCol=vCol, vCol=vCol, vertex.label.family="Helvetica")
title(
	'Nigerian Intra-State Conflict Post-Boko Haram\n(2009-2016)', 
	family='Helvetica', adj=1, line=-4)
dev.off()
################