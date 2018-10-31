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

# set up pos mat
tmp=getGraphfromMat(mat=yArrSum)
gArrSum=tmp$g
gArrPos=tmp$nodePos
rm(tmp)
################

################
# plot by full, pre and post bh periods
fName = paste0('floats/figure3.pdf')
pdf(file=fName, width=13,height=9)
vCol = ifelse(names(V(gArrSum)) %in% govActors, 'gray30', 'gray95')
vLabCol = ifelse(names(V(gArrSum)) %in% govActors, 'white', 'gray30')
set.seed(6886)
plotGraph(gArrSum, gArrPos, 
	vShape='circle', vLabCex=gArrSum$labSize+.3, 
	vLabCol=vLabCol, vertex.label.font=1, vertex.size=gArrSum$vSize, 
	vFrameCol=vCol, vCol=vCol, vertex.label.family="Helvetica")
title('Nigerian Intra-State Conflict\n(2000-2016)', family='Helvetica', adj=1, line=-4)	
dev.off()
################