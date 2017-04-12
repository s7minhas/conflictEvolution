################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
source(paste0(fPth, 'ggCirc.R'))
source(paste0(fPth, 'actorInfo.R'))
################

################
# load data
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
yrs = char(2000:2016) ; yList = yList[yrs]

load(paste0(pathResults, 'ameResults.rda')) # load AME mod results
yArr = listToArray(actors=sort(unique(unlist(lapply(yList,rownames)))), 
	Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
yArrSumm = apply(yArr, c(1,2), sum, na.rm=TRUE)
diag(yArrSumm) = 0

# adjust actor names

vNameKey = getNameKey(yList)
rownames(yArrSumm) = vNameKey$clean[match(rownames(yArrSumm), vNameKey$dirty)]
colnames(yArrSumm) = vNameKey$clean[match(colnames(yArrSumm), vNameKey$dirty)]
rownames(fitFullSpec$U) = vNameKey$clean[match(rownames(fitFullSpec$U), vNameKey$dirty)]
rownames(fitFullSpec$V) = vNameKey$clean[match(rownames(fitFullSpec$V), vNameKey$dirty)]
################

################
uvCols = brewer.pal(11, 'RdBu')[c(11-2, 3)]
circPlot=ggCirc(
	Y=yArrSumm, U=fitFullSpec$U, V=fitFullSpec$V, vscale=.6, 
	family="Source Sans Pro Light", force=3, 
	lcol='gray85', lsize=.05) +
	scale_color_manual(values=uvCols)
ggsave(circPlot, file=paste0(pathGraphics,'circPlot.pdf'), width=12, height=10, device=cairo_pdf)
################