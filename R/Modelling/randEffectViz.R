################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
################

################
# load data
load(paste0(pathResults, 'ameResults.rda')) # load AME mod results
################

################
# some quick viz checks
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
addEffData = data.frame(APM=fit$APM, BPM=fit$BPM, stringsAsFactors = FALSE)
addEffData$actor = rownames(addEffData) ; rownames(addEffData) = NULL
addEffData$actor = factor(addEffData$actor, levels=addEffData[order(addEffData$BPM),'actor'])
ggplot(addEffData, aes(x=actor, y=BPM)) +
	geom_point() +
	ylab('Additive Sender Effects') + xlab('')
	theme(
		panel.border=element_blank(), axis.ticks=element_blank(),
		axis.text.x=element_text(angle=45, hjust=1, size=4)
		)

addEffData_preBH = data.frame(APM=fitPreBH$APM, BPM=fitPreBH$BPM, stringsAsFactors = FALSE)
addEffData_preBH$actor = rownames(addEffData_preBH) ; rownames(addEffData_preBH) = NULL

addEffData_postBH = data.frame(APM=fitPostBH$APM, BPM=fitPostBH$BPM, stringsAsFactors = FALSE)
addEffData_postBH$actor = rownames(addEffData_postBH) ; rownames(addEffData_postBH) = NULL
################

################
# multiplicative effects
yArr = listToArray(actors=sort(unique(unlist(lapply(yList,rownames)))), Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
yArrSumm = apply(yArr, c(1,2), sum, na.rm=TRUE)
circplot(Y=yArrSumm, U=fit$U, V=fit$V, pscale=.7)
################