################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
library(amen)
################

################
# load data
load(paste0(pathResults, 'ame_shiftTimeDummy.rda'))
load(paste0(pathResults, 'ameResults.rda'))
################

################
# pull out time dummy param
bokParam = do.call('cbind', lapply(fitPostBokoMods, function(x){
  return(x$BETA[,ncol(x$BETA)])
}))
colnames(bokParam) = c(paste0('tminus',1:5),'t',paste0('tplus',1:5))

# organize
summStats = function(x){c(
  quantile(x,probs=c(0.025,0.05,0.5,0.95,0.975))
  )}
bokSumm = data.frame(t(apply(bokParam, 2, summStats)))
colnames(bokSumm) = c('lo95','lo90','med','hi90','hi95')
bokSumm$Var = c(paste0('T-',1:5),'T',paste0('T+',1:5))
bokSumm$Var = factor(bokSumm$Var,
     levels=c(paste0('T-',5:1),'T',paste0('T+',1:5)))
################

################
timeParamCompare = ggplot(bokSumm, aes(x=Var)) + 
  geom_point(aes(y=med)) + 
  geom_linerange(aes(ymin=lo95,ymax=hi95), lwd=.5) +
  geom_linerange(aes(ymin=lo90,ymax=hi90), lwd=1) +
  geom_hline(yintercept = 0, color='red') +
  ylab('Parameter Estimate') + 
  xlab('T=Time when\nBoko Haram\nEntered Network') +
  theme_bw() + 
  theme(
    panel.border=element_blank(),
    axis.ticks=element_blank(),
    axis.text = element_text(family="Source Sans Pro Light"),
    axis.title = element_text(family="Source Sans Pro")
  )
ggsave(timeParamCompare, file=paste0(pathGraphics,'timeParamCompare.pdf'), width=7, height=4, device=cairo_pdf)
################