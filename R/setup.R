# Clear workspace
rm(list=ls())

####################################
# Set up paths
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	dpth='~/Dropbox/Research/conflictEvolution/';
	gpth='~/Research/conflictEvolution/';
	pathData=paste0(dpth, 'data/');
	pathGraphics=paste0(dpth, 'graphics/')
	pathResults = paste0(dpth, 'results/')
}

if(Sys.info()["user"]=="cassydorff"){
	dpth='~/Dropbox/Research/conflictEvolution/'
	gpth='~/ProjectsGit/conflictEvolution/'
	pathData=paste0(dpth, 'data/');
	pathGraphics=paste0(dpth, 'graphics/')
}
####################################

####################################
# Load helpful libraries
loadPkg=function(toLoad){
    for(lib in toLoad){
      if(!(lib %in% installed.packages()[,1])){ 
        install.packages(lib, repos='http://cran.rstudio.com/') }
      library(lib, character.only=TRUE) } }

toLoad = c('amen', 'magrittr', 'foreach', 'doParallel', 
	'network', 'igraph', 'ggplot2', 'reshape2')
loadPkg(toLoad)

## gg theme
theme_set(theme_bw())
####################################

####################################
# Helpful functions
pasteVec = function(x,y){ as.vector( outer( x, y, paste0 ) ) }
char = function(x) { as.character(x) }
num = function(x) { as.numeric(char(x)) }
cname = function(x) { countrycode(x, 'country.name', 'country.name') }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
substrRight = function(x, n){ substr(x, nchar(x)-n+1, nchar(x)) }
summStats = function(x){
	mu=mean(x)
	qts=quantile(x, probs=c(0.025, 0.975, 0.05, 0.95))
	return( c(mu, qts) ) }

# Trace plot
getTracePlot = function(mod){
	beta = mod$'BETA' %>% data.frame()
	beta$iter = 1:nrow(beta)
	ggData = melt(beta, id='iter')
	tracePlot = ggplot(ggData, aes(x=iter, y=value)) + geom_line() + facet_wrap(~variable, scales='free_y', ncol=1)	
	return(tracePlot) }

# coef data
getCoefData = function(ii, mods, modSpec){
	# Generate data for coefficient plot
	mod = mods[[ii]]
	beta = mod$'BETA' %>% data.frame()
	beta = beta[-(1:((toBurn-1)/odes)),,drop=FALSE]
	betaSumm = t ( apply(beta, 2, function(x){
		c( mean(x), quantile(x, probs=c(0.025, 0.975, 0.05, 0.95) ) ) }) )
	ggData = data.frame( betaSumm, row.names = NULL )
	names(ggData) = c('mean', pasteVec(c('lo','hi'), c('95','90')) )
	ggData$var = rownames( betaSumm )
	ggData$mod = modSpec[[ii]]$'name'
	# Generate colors
	ggData$sig = NA
	ggData$sig[ggData$lo90 > 0 & ggData$lo95 < 0] = "Positive at 90"
	ggData$sig[ggData$lo95 > 0] = "Positive"
	ggData$sig[ggData$hi90 < 0 & ggData$hi95 > 0] = "Negative at 90"
	ggData$sig[ggData$hi95 < 0] = "Negative"
	ggData$sig[ggData$lo90 < 0 & ggData$hi90 > 0] = "Insig"
	return(ggData) }

# coef plot
getCoefPlot = function(
	ggData, dropIntercept=TRUE, 
	replaceNodeName = 'protestLagCount.node', 
	vars=c('dto.node', 'centrality.node', 'betweeness.node', 'protestLagCount.node'),
	varLabels=c('DTO', 'Centrality', 'Betweeness', 'Protest'),
	mods=c('protest','net','protest_net','protest_net_dto', 'dtoF_protest_net'),
	modsToKeep=c('protest','net','protest_net','protest_net_dto', 'dtoF_protest_net'),
	modLabels=c('Protest', 'Network', 'Protest + Net', 'Protest + Net + DTO', 'Protest + Net + I(DTO)'),
	facetRows=1
	 ){

	# Colors for coef plot
	coefCols = c("Positive"=rgb(54, 144, 192, maxColorValue=255), 
	                "Negative"= rgb(222, 45, 38, maxColorValue=255),
	                "Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
	                "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
	                "Insig" = rgb(150, 150, 150, maxColorValue=255))	

	# var names and facet ordering
	if(dropIntercept){ ggData = ggData[ggData$var!='intercept',] }
	ggData$var = char( ggData$var )
	ggData$var[ggData$var=='.node'] = replaceNodeName
	for(ii in 1:length(vars)){ ggData$var[ggData$var==vars[ii]] = varLabels[ii] }
	ggData$mod = char(ggData$mod)
	ggData = ggData[which(ggData$mod %in% modsToKeep),]
	for(ii in 1:length(mods)){ ggData$mod[ggData$mod==mods[ii]] = modLabels[ii] }
	ggData$var = factor(ggData$var, levels=varLabels)
	ggData$mod = factor(ggData$mod, levels=modLabels)

	# make plot
	ggCoef=ggplot(ggData, aes(x=var, y=mean, color=sig)) 
	ggCoef=ggCoef + geom_hline(yintercept=0, color='red', linetype='dashed')	
	ggCoef = ggCoef + geom_linerange(aes(ymin=lo95, ymax=hi95), alpha = .5, size = 0.7)
	ggCoef = ggCoef + geom_linerange(aes(ymin=lo90, ymax=hi90),alpha = 1, size = 1) + geom_point(size=2)
	ggCoef=ggCoef + xlab('') + ylab('') + scale_colour_manual(values = coefCols)
	ggCoef=ggCoef + theme(
		axis.ticks=element_blank(),
		panel.background = element_blank(),
		legend.position='none',
		panel.grid.major=element_blank(),
		panel.grid.minor=element_blank() )
	ggCoef = ggCoef + coord_flip() + facet_wrap(~mod, nrow=facetRows)
	return(ggCoef) }
####################################