################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
################

################
# some helper fns
plotAddEff = function(fit, row=TRUE){
	if(row){addEffData = data.frame(addEff=fit$APM, stringsAsFactors = FALSE) ; yLabel='Sender Effects'}
	if(!row){addEffData = data.frame(addEff=fit$BPM, stringsAsFactors = FALSE) ; yLabel='Receiver Effects'}
	addEffData$actor = rownames(addEffData) ; rownames(addEffData) = NULL
	addEffData$actor = factor(addEffData$actor, levels=addEffData[order(addEffData$addEff),'actor'])
	addEffData$max = ifelse(addEffData$addEff>=0,addEffData$addEff,0)
	addEffData$min = ifelse(addEffData$addEff<0,addEffData$addEff,0) 
	gg = ggplot(addEffData, aes(x=actor, y=addEff)) +
		geom_point() + geom_linerange(aes(ymax=max,ymin=min)) +
		ylab(yLabel) + xlab('') + 
		geom_hline(yintercept=0,color='red') + 
		theme(
			panel.border=element_blank(), axis.ticks=element_blank(),
			# axis.text.x=element_text(angle=45, hjust=1, size=4)
			axis.text.x=element_text(angle=90, hjust=1, size=6)
			)
	return(gg)
}
################

################
# load data
load(paste0(pathResults, 'ameResults.rda')) # load AME mod results
################

################
# some quick viz checks
paramPlot(fitDyadCovar$BETA)
paramPlot(fitDyadCovar$VC[,-ncol(fitDyadCovar$VC)])
gofPlot(fitDyadCovar$GOF, FALSE)

paramPlot(fit$BETA)
paramPlot(fit$VC[,-ncol(fit$VC)])
gofPlot(fit$GOF, FALSE)

paramPlot(fitPreBH$BETA)
paramPlot(fitPreBH$VC[,-ncol(fitPreBH$VC)])
gofPlot(fitPreBH$GOF, FALSE)

paramPlot(fitPostBH$BETA)
paramPlot(fitPostBH$VC[,-ncol(fitPostBH$VC)])
gofPlot(fitPostBH$GOF, FALSE)
################

################
# additive effects
plotAddEff(fitDyadCovar, TRUE)
plotAddEff(fitDyadCovar, FALSE)

plotAddEff(fit, TRUE)
plotAddEff(fit, FALSE)

plotAddEff(fitPreBH, TRUE)
plotAddEff(fitPreBH, FALSE)

plotAddEff(fitPostBH, TRUE)
plotAddEff(fitPostBH, FALSE)
################

################
# multiplicative effects
yArr = listToArray(actors=sort(unique(unlist(lapply(yList,rownames)))), Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
yArrSumm = apply(yArr, c(1,2), sum, na.rm=TRUE)

circplot(Y=yArrSumm, U=fitDyadCovar$U, V=fitDyadCovar$V, pscale=.7)
circplot(Y=yArrSumm, U=fitDyadCovar$U, V=NULL, pscale=.7)
circplot(Y=yArrSumm, U=fitDyadCovar$V, V=NULL, pscale=.7)

circplot(Y=yArrSumm, U=fit$U, V=fit$V, pscale=.7)

circplot(Y=yArrSumm, U=fit$U, V=fit$V, pscale=.7)

circplot(Y=yArrSumm, U=fitPreBH$U, V=fitPreBH$V, pscale=.7)

circplot(Y=yArrSumm, U=fitPostBH$U, V=fitPostBH$V, pscale=.7)
################