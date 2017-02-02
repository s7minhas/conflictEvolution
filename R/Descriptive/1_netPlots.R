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
################

################
# load data
load(paste0(pathResults, 'ameResults.rda')) # load AME mod results with yList used for modeling
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

# plot by full, pre and post bh periods
fName = paste0(pathGraphics, 'nigeria_2000_2016.pdf') ; pdf(file=fName, width=13,height=9)
plotGraph(gArrSum, gArrPos, 
	main='Nigerian Intra-State Conflict\n(2000-2016)', vLabCex=.7, vertex.size=9)
dev.off() ; system(paste('pdfcrop',fName,fName, sep=' '))

fName = paste0(pathGraphics, 'nigeria_preBK.pdf') ; pdf(file=fName, width=13,height=9)
plotGraph(gArrSumPreBH, gArrPosPreBH, 
	main='Nigerian Intra-State Conflict Pre-Boko Haram\n(2000-2008)', vLabCex=.7, vertex.size=9)
dev.off() ; system(paste('pdfcrop',fName,fName, sep=' '))

fName = paste0(pathGraphics, 'nigeria_postBK.pdf') ; pdf(file=fName, width=13,height=9)
plotGraph(gArrSumPostBH, gArrPosPostBH, 
	main='Nigerian Intra-State Conflict Post-Boko Haram\n(2009-2016)', vLabCex=.7, vertex.size=9)
dev.off() ; system(paste('pdfcrop',fName,fName, sep=' '))

# plot by t
selT = rep(FALSE, dim(yArr)[3])
lapply(1:length(yList), function(t){
	selT[t] = TRUE
	tmp = getGraphfromMat(
		mat=getMatfromArr(yArr, pds=selT), nodePos=gArrPos,
		colBrksByDegree=seq(0,12,3),
		)
	g = tmp$g ; pos = tmp$nodePos ; rm(tmp)
	fName = paste0(pathGraphics, 'nigeriaT_', names(yList)[t], '.jpg')
	jpeg(filename=fName, width=1500,height=900)
	plotGraph(g, pos, 
		main=paste0('Nigerian Intra-State Conflict\n(',names(yList)[t],')'), 
		vertex.size=300,
		vLabCex=g$labSize+.5,
		arrowSize=2,
		aspLogic=0, rescale=FALSE,
		ylim=c(min(gArrPos[,2]), max(gArrPos[,2])),
		xlim=c(min(gArrPos[,1]), max(gArrPos[,1])),
		)
	dev.off()
})
setwd(pathGraphics)
system("convert -delay 50 nigeriaT_*.jpg nigeriaConf.mov ; rm -rf nigeriaT_*.jpg")
################