################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
################

################
# load data
load(paste0(pathResults, 'ameResults.rda')) # load AME mod results
yArr = listToArray(actors=sort(unique(unlist(lapply(yList,rownames)))), 
	Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
################

################
paramPlot(fitFullSpec$VC[,-ncol(fitFullSpec$VC)])
gofPlot(fitFullSpec$GOF[,3:4], FALSE)
################

################
# multiplicative effects
yArrSumm = apply(yArr, c(1,2), sum, na.rm=TRUE)
circplot(Y=yArrSumm, U=fitFullSpec$U, V=fitFullSpec$V, pscale=.7)
circplot(Y=yArrSumm, U=fitFullSpec$U, V=NULL, pscale=.7)
circplot(Y=yArrSumm, U=fitFullSpec$V, V=NULL, pscale=.7)
################