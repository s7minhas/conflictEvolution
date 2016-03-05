# setup
source('~/Research/conflictEvolution/R/setup.R')

# Load output files into list
iter=10000 ; toBurn=1001 ; odes=10
load(file=paste0(pathResults, 'modList.rda'))

binMods = lapply(names(modList), function(mod){ load( paste0(pathResults,mod,'_bin.rda') ) ; return(fit) }) ; names(binMods) = names(modList)
tracePlotsBin = lapply(binMods, getTracePlot)
coefDataBin = lapply(1:length(binMods), function(ii){ getCoefData(ii, binMods, modList) } ) %>% do.call('rbind', .)
getCoefPlot( coefDataBin )

ordMods = lapply(names(modList), function(mod){ load( paste0(pathResults,mod,'_ord.rda') ) ; return(fit) }); names(ordMods) = names(ordMods)
tracePlotsOrd = lapply(ordMods, getTracePlot)
coefDataOrd = lapply(1:length(ordMods), function(ii){ getCoefData(ii, ordMods, modList) } ) %>% do.call('rbind', .)
getCoefPlot( coefDataOrd )