################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
source(paste0(fPth, 'actorInfo.R'))
loadPkg('dplyr')
################

################
# load data
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
yrs = char(2000:2016) ; yList = yList[yrs]
# pull out array
yArr = listToArray(actors=getActor(yList), Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y

# create dyadic version
yDF = melt(yList) ; yDF = yDF[yDF$value!=0,]
aKey = getNameKey(yList)
yDF$Var1 = aKey$clean[match(yDF$Var1,aKey$dirty)]
yDF$Var1 = gsub('\n',' ', yDF$Var1, fixed=TRUE)
################

################
# get total number of conf by T
unlist(lapply(yList, function(x){c(sum(x,na.rm=TRUE))}))
################

################
# calc some net stats over time
netSummStats = data.frame(do.call('rbind', lapply(yList, gofstats)))
netSummStats$year = rownames(netSummStats)

ggplot(melt(netSummStats), aes(x=year, y=value, group=1)) +
	geom_bar(stat='identity') +
	facet_wrap(~variable, nrow=2, scales='free_y') +
	xlab('') + ylab('') + 
	theme(
		axis.text.x = element_text(angle=45, hjust=1),
		axis.ticks=element_blank(),
		panel.border = element_blank()
		)

netSummStats$year = num(netSummStats$year)
yKey = data.frame(t=2001:2016, lab=cut(2001:2016,4))
netSummStats = netSummStats[-1,]
netSummStats$pd = yKey$lab[match(netSummStats$year,yKey$t)]
ggNetSumm = netSummStats[,-which(names(netSummStats) %in% c('year'))] %>%
	group_by(pd) %>%
	summarise_each(funs(mean)) %>%
	melt(.,id='pd')

ggplot(ggNetSumm, aes(x=pd, y=value, group=1)) +
	geom_bar(stat='identity') +
	facet_wrap(~variable, nrow=2, scales='free_y') +
	xlab('') + ylab('') + 
	theme(
		axis.text.x = element_text(angle=45, hjust=1),
		axis.ticks=element_blank(),
		panel.border = element_blank()
		)	

# degree dists
degreeDF = do.call('rbind', lapply(unique(yDF$L1), function(t){
	cbind(
		data.frame(table( yDF$Var1[yDF$L1==t] )),
		data.frame(table( yDF$Var2[yDF$L1==t] ))[,2], year=t )
}))
names(degreeDF)[2:3] = c('outDegree','inDegree')
degreeDF$degree = degreeDF$outDegree + degreeDF$inDegree

summ = degreeDF %>% group_by(Var1) %>% summarise(degT=sum(degree))
groupOrder = char(summ$Var1[order(summ$degT,decreasing=TRUE)])

ggDegree = melt(degreeDF)
ggDegree$Var1 = factor(ggDegree$Var1, levels=rev(groupOrder))

ggDegree = ggDegree[ggDegree$variable!='degree',]
ggplot(ggDegree, aes(x=Var1, y=value, color=year, fill=year)) +
	# geom_point() + 
	# geom_linerange(aes(ymin=0,ymax=value)) +
	facet_wrap(~variable, scales='free_y', nrow=2) +
	geom_bar(stat='identity', position='stack') +
	xlab('') + ylab('') + 
	theme(
		axis.text.x = element_text(angle=45, hjust=1),
		axis.ticks=element_blank(),
		panel.border = element_blank()
		)
################