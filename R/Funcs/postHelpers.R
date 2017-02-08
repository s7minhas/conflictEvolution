# calc CI ranges, beta df must have mean and sd cols
getCIVecs = function(beta){
	beta$lo95 = beta$mean - qnorm(.975)*beta$sd
	beta$hi95 = beta$mean + qnorm(.975)*beta$sd
	beta$lo90 = beta$mean - qnorm(.95)*beta$sd  
	beta$hi90 = beta$mean + qnorm(.95)*beta$sd
	return(beta)
}

# colors for sig
coefp_colors = c(
	"Positive"=rgb(54, 144, 192, maxColorValue=255),
	"Negative"= rgb(222, 45, 38, maxColorValue=255),
	"Positive at 90"=rgb(158, 202, 225, maxColorValue=255),
	"Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
	"Insignificant" = rgb(150, 150, 150, maxColorValue=255)
	)

# add sig col to beta df gen from getCIVecs
getSigVec = function(beta){
	beta$sig = NA
	beta$sig[beta$lo90 > 0 & beta$lo95 < 0] = "Positive at 90"
	beta$sig[beta$lo95 > 0] = "Positive"
	beta$sig[beta$hi90 < 0 & beta$hi95 > 0] = "Negative at 90"
	beta$sig[beta$hi95 < 0] = "Negative"
	beta$sig[beta$lo90 < 0 & beta$hi90 > 0] = "Insignificant"
	return(beta)
}

# AB Effect plot
addEffPlot = function(fit, row=TRUE, addDegree=FALSE, yList=NULL, orderByDegree=FALSE){
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