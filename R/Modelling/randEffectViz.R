################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
################

################
# some helper fns
plotAddEff = function(fit, row=TRUE, addDegree=FALSE, yList=NULL, orderByDegree=FALSE){
	if(row){addEffData = data.frame(addEff=fit$APM, stringsAsFactors = FALSE) ; yLabel='Sender Effects'}
	if(!row){addEffData = data.frame(addEff=fit$BPM, stringsAsFactors = FALSE) ; yLabel='Receiver Effects'}
	addEffData$actor = rownames(addEffData) ; rownames(addEffData) = NULL
	if(!orderByDegree){
		addEffData$actor = factor(addEffData$actor, 
			levels=addEffData[order(addEffData$addEff),'actor'])
	}
	if(addDegree){
		yArr = listToArray(
			actors=sort(unique(unlist(lapply(yList,rownames)))), 
			Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
		if(row){ degree = sort(apply(yArr, 1, mean, na.rm=TRUE)) }
		if(!row){ degree = sort(apply(yArr, 2, mean, na.rm=TRUE)) }
		if(orderByDegree){ 
			addEffData$actor = factor(addEffData$actor, 
				levels=names(degree) )
		}
		addEffData$var = 'Additive Effect'
		tmp = addEffData ; tmp$addEff = degree[match(tmp$actor,names(degree))] ; tmp$var=' Avg. Degree'
		addEffData = rbind(addEffData, tmp) ; rm(tmp)
	}
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
	if(addDegree){
		gg = gg + facet_wrap(~var, nrow=2, scales='free_y')
	}
	return(gg)
}
################

################
# load data
load(paste0(pathResults, 'ameResults.rda')) # load AME mod results
yArr = listToArray(actors=sort(unique(unlist(lapply(yList,rownames)))), 
	Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
################

################
# some quick viz checks
paramPlot(fitDyadCovar$BETA)
paramPlot(fitDyadCovar$VC[,-ncol(fitDyadCovar$VC)])
gofPlot(fitDyadCovar$GOF[,3:4], FALSE)
################

################
# multiplicative effects
yArrSumm = apply(yArr, c(1,2), sum, na.rm=TRUE)
circplot(Y=yArrSumm, U=fitDyadCovar$U, V=fitDyadCovar$V, pscale=.7)
circplot(Y=yArrSumm, U=fitDyadCovar$U, V=NULL, pscale=.7)
circplot(Y=yArrSumm, U=fitDyadCovar$V, V=NULL, pscale=.7)
################

################
# additive effects
plotAddEff(fitDyadCovar, row=TRUE, addDegree=TRUE, yList, orderByDegree=FALSE)
plotAddEff(fitDyadCovar, row=FALSE, addDegree=TRUE, yList, orderByDegree=FALSE)
################