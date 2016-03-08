# setup
source('~/Research/conflictEvolution/R/setup.R')

# Modify pathResults based on dv
# pathResults ='~/Dropbox/Research/conflictEvolution/results/' # use for dv where gov-gov = 0
pathResults ='~/Dropbox/Research/conflictEvolution/results/all_interactions/' # use for dv where gov-gov!=0
if(grepl('all_interactions',pathResults)){ pname = 'all_' } else { pname = 'noGov_gov_' }


# Load output files into list
iter=10000 ; toBurn=1001 ; odes=10
load(file=paste0(pathResults, 'modList.rda'))

binMods = lapply(names(modList), function(mod){ load( paste0(pathResults,mod,'_bin.rda') ) ; return(fit) }) ; names(binMods) = names(modList)
tracePlotsBin = lapply(binMods, getTracePlot)
coefDataBin = lapply(1:length(binMods), function(ii){ getCoefData(ii, binMods, modList) } ) %>% do.call('rbind', .)

binCoef = getCoefPlot(
	ggData = coefDataBin,
	modsToKeep = c('protest','net','protest_net','protest_net_dto' ),
	facetRows=2 )	
ggsave(filename=paste0(pathResults, pname, 'bin.pdf'), height=6, width=8)

ordMods = lapply(names(modList), function(mod){ load( paste0(pathResults,mod,'_ord.rda') ) ; return(fit) }); names(ordMods) = names(modList)
tracePlotsOrd = lapply(ordMods, getTracePlot)
coefDataOrd = lapply(1:length(ordMods), function(ii){ getCoefData(ii, ordMods, modList) } ) %>% do.call('rbind', .)

ordCoef = getCoefPlot( 
	ggData=coefDataOrd, 
	modsToKeep=c('protest','net','protest_net','protest_net_dto' ),
	facetRows=2 )
ggsave(filename=paste0(pathResults, pname, 'ord.pdf'), height=6, width=8)