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
yDF$Var2 = aKey$clean[match(yDF$Var2,aKey$dirty)]
yDF$Var2 = gsub('\n',' ', yDF$Var2, fixed=TRUE)
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
################	

################
# degrees  by t and actor
getDegreeDF = function(dyad, actors, years){
	inDegree = data.frame(table( dyad$Var2, dyad$L1 ))
	outDegree = data.frame(table( dyad$Var1, dyad$L1 ))
	degreeDF = expand.grid(actor=actors, year=years)
	degreeDF$actor = gsub('\n',' ', degreeDF$actor, fixed=TRUE)
	degreeDF$inDegree = inDegree$Freq[match(
		paste0(degreeDF$actor,degreeDF$year),
		paste0(inDegree$Var1,inDegree$Var2)
		)] ; degreeDF$inDegree[is.na(degreeDF$inDegree)] = 0
	degreeDF$outDegree = outDegree$Freq[match(
		paste0(degreeDF$actor,degreeDF$year),
		paste0(outDegree$Var1,outDegree$Var2)
		)] ; degreeDF$outDegree[is.na(degreeDF$outDegree)] = 0
	degreeDF$degree = degreeDF$outDegree + degreeDF$inDegree
	return(degreeDF)
}
degreeDF = getDegreeDF(yDF, aKey$clean, 2000:2016)

# across T by actor
summ = degreeDF[,-which(names(degreeDF) %in% c('year'))] %>%
	group_by(actor) %>% summarise_each(funs(sum))
groupOrder = char(summ$actor[order(summ$degree,decreasing=TRUE)])
ggDegree = melt(summ)
ggDegree$actor = factor(ggDegree$actor, levels=groupOrder)
ggDegree = ggDegree[ggDegree$variable!='degree',]
ggplot(ggDegree, aes(x=actor, y=value)) +
	geom_point() + 
	geom_linerange(aes(ymin=0,ymax=value)) +
	facet_wrap(~variable, scales='free_y', nrow=2) +
	# geom_bar(stat='identity', position='stack') +
	xlab('') + ylab('') + 
	theme(
		axis.text.x = element_text(angle=45, hjust=1),
		axis.ticks=element_blank(),
		panel.border = element_blank()
		)

# focus on actor by T
degreeDF_noBH = getDegreeDF(
	yDF[which(!yDF$Var1=='Boko Haram' & !yDF$Var2=='Boko Haram'),],
	aKey$clean, 2000:2016)

aFocus = c('Police (Nigeria)','Military (Nigeria)')
aSlice = degreeDF[which(degreeDF$actor %in% aFocus),]
aSlice_noBH = degreeDF_noBH[which(degreeDF_noBH$actor %in% aFocus),]
colnames(aSlice_noBH)[3:5] = paste0(colnames(aSlice_noBH)[3:5],'_noBH')
aSlice = cbind(aSlice, aSlice_noBH[,3:5])
aSlice = melt(aSlice, id=c('actor','year'))

aSlice$variable = char(aSlice$variable)
aSlice$BH = unlist(lapply(strsplit(aSlice$variable,'_',fixed=TRUE), function(x){x[2]}))
aSlice$BH[is.na(aSlice$BH)] = 'all'
aSlice$variable = unlist(lapply(strsplit(aSlice$variable,'_',fixed=TRUE), function(x){x[1]}))

ggplot(
		aSlice[aSlice$variable!='degree',], 
		aes(x=year, y=value, color=BH, fill=BH)
	) +
	# geom_point(position=position_dodge(width=.5)) + 
	# geom_linerange(aes(ymin=0,ymax=value),position=position_dodge(width=.5)) + 
	geom_bar(stat='identity',position=position_dodge(width=.7)) +
	facet_grid(actor ~ variable) +
	xlab('') + ylab('') + 
	theme(
		axis.text.x = element_text(angle=45, hjust=1),
		axis.ticks=element_blank(),
		panel.border = element_blank()
		)	
################