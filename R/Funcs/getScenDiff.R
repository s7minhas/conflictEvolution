##################################################################
loadPkg(c('dplyr','magrittr','plyr','tidyr'))
# scen calc
# calc diffs
getScenDiff = function(
	linkType='logit', # logit or probit
	scenHi, scenLo, scenNames, 
	beta, modName, type='summStats' # summStats or density
	){
	if(linkType=='logit'){
		predHi = 1/(1+exp( -t(t(scenHi) %*% t(beta))) )
		predLo = 1/(1+exp(-t(t(scenLo) %*% t(beta)))) }
	if(linkType=='probit'){
		predHi = pnorm(t(t(scenHi) %*% t(beta)))
		predLo = pnorm(t(t(scenLo) %*% t(beta))) }
	predDiff = predHi-predLo
	colnames(predDiff) = scenNames

	if(type=='summStats'){
		summPred = matrix(NA,nrow=5, ncol=ncol(predDiff),
			dimnames=list(
				c('med','hi95','hi90','lo95','lo90'), colnames(predDiff) ))
		for(s in colnames(summPred)){
			summPred['med',s]=median(predDiff[,s])
			summPred['hi95',s]=quantile(predDiff[,s],.975)
			summPred['hi90',s]=quantile(predDiff[,s],.95)
			summPred['lo95',s]=quantile(predDiff[,s],.025)
			summPred['lo90',s]=quantile(predDiff[,s],.05) }
	
		# org and spit
		summPred = t(summPred) %>% data.frame() %>%
			mutate(
				mod=modName,
				scen=colnames(summPred) )
		}

	if(type=='density'){
		summPred = predDiff %>% data.frame() %>%
			gather(key='scen',value='value') %>%
			mutate(
				mod=modName,
				scenClean=gsub('.',' ', scen, fixed=TRUE)
				)
		}

	if(type=='densityShade'){
		predDiffMelt=gather(data.frame(predDiff), key='scen', value='value')
		ggMeans = predDiffMelt %>% group_by(scen) %>% dplyr::summarise(sMean=mean(value))
		summPred = plyr::ddply(predDiffMelt, .(scen), .fun=function(x){
			tmp = density(x$value); x1 = tmp$x; y1 = tmp$y
			q95 = x1 >= quantile(x$value,0.025) & x1 <= quantile(x$value,0.975)
			q90 = x1 >= quantile(x$value,0.05) & x1 <= quantile(x$value,0.95)
			data.frame(x=x1,y=y1,q95=q95, q90=q90) } )
		summPred$mean = ggMeans$sMean[match(summPred$scen,ggMeans$scen)]
		summPred$mod=modName
		}

	return(summPred)
}
##################################################################