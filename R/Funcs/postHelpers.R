# Construct coefficient plot
buildCoef = function(coefData, plot=TRUE){
	# Depends on latex2exp and ggplot
	
	# Add conf ints
	coefData$up95 = coefData$est + qnorm(.975)*coefData$se
	coefData$lo95 = coefData$est - qnorm(.975)*coefData$se
	coefData$up90 = coefData$est + qnorm(.95)*coefData$se
	coefData$lo90 = coefData$est - qnorm(.95)*coefData$se

	# Add in variable for colors
	coefData$sig = NULL
	coefData$sig[coefData$lo90 > 0 & coefData$lo95 < 0] = "Positive at 90"
	coefData$sig[coefData$lo95 > 0] = "Positive"
	coefData$sig[coefData$up90 < 0 & coefData$up95 > 0] = "Negative at 90"
	coefData$sig[coefData$up95 < 0] = "Negative"
	coefData$sig[coefData$lo90 < 0 & coefData$up90 > 0] = "Insig"
	coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255), 
	                "Negative"= rgb(222, 45, 38, maxColorValue=255),
	                "Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
	                "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
	                "Insig" = rgb(150, 150, 150, maxColorValue=255))

	# Create coefficient plot
	mathLabs = levels(coefData$var)
	ggCoef = ggplot(coefData, aes(var, est, color = sig)) +
		geom_linerange(aes(ymin=lo95, ymax=up95), alpha = .3, size = 0.3) + 
		geom_linerange(aes(ymin=lo90, ymax=up90),alpha = 1, size = 1) + 
		geom_hline(aes(yintercept=0), linetype=2, color = "black") + 
		geom_point(aes(as.factor(var),est), size=4, shape=20) + 
		geom_errorbar(aes(ymin=lo95,ymax=up95),linetype = 1,width = 0.1) +
		scale_colour_manual(values = coefp_colors) +
		scale_x_discrete(labels=unlist(lapply(mathLabs, TeX))) +
		coord_flip() + xlab("") + ylab("") +
		theme(
			axis.ticks=element_blank(), panel.border=element_blank(),
			legend.position='none'
			)
	if(plot){ ggCoef } else { coefData }
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