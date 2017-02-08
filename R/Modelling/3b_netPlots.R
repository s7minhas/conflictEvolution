################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
source(paste0(fPth, 'netPlotHelpers.R'))
################

################
# load data
load(paste0(pathResults, 'ameResults.rda')) # load AME mod results
yArr = listToArray(actors=sort(unique(unlist(lapply(yList,rownames)))), 
	Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
yArrSumm = apply(yArr, c(1,2), sum, na.rm=TRUE)

# adjust actor names
vNameKey = getNameKey(yList)
rownames(yArrSumm) = vNameKey$clean[match(rownames(yArrSumm), vNameKey$dirty)]
colnames(yArrSumm) = vNameKey$clean[match(colnames(yArrSumm), vNameKey$dirty)]
rownames(fitFullSpec$U) = vNameKey$clean[match(rownames(fitFullSpec$U), vNameKey$dirty)]
rownames(fitFullSpec$V) = vNameKey$clean[match(rownames(fitFullSpec$V), vNameKey$dirty)]
################

################
# multiplicative effects
circplot(Y=yArrSumm, U=fitFullSpec$U, V=fitFullSpec$V,
	pscale=1, 
	rcol='black', ccol='black', jitter=.2,
	lcol='gray80')
################

################
uvCols = brewer.pal(11, 'RdBu')[c(11-2, 3)]
circPlot=ggCirc(
	Y=yArrSumm, U=fitFullSpec$U, V=fitFullSpec$V,
	vscale=.6, family="Source Sans Pro Light",
	lcol='gray90', lsize=.1) +
	scale_color_manual(values=uvCols)
ggsave(circPlot, file=paste0(pathGraphics,'circPlot.pdf'), width=10, height=8, device=cairo_pdf)
################