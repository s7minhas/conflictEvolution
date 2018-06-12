################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
source(paste0(fPth, 'actorInfo.R'))
loadPkg(c('dplyr','abind','ggrepel','fossil'))
################

################
# load data
load(paste0(pathData, "nData.rda"))
nData = nData[
	which(nData$YEAR %in% 2001:2016),
	# c('YEAR','a1','a2','LATITUDE','LONGITUDE')
	]
################

################
# map
loadPkg(c('cshapes', 'ggmap','countrycode'))

# get cntry shape from cshapes
worldmap=cshapes::cshp(date=as.Date("2005-01-01"),useGW=F)
cntryShape = worldmap[worldmap$COWCODE==countrycode("NIGERIA","country.name","cown"),]
gpclibPermit() ; nigShape = fortify(cntryShape, region = "COWCODE")

# get admin lines from ggmap
if( !file.exists(paste0(pathData, 'nigeriaStamenLines.rda'))  ){
	ngaLines = ggmap::get_map(location='Nigeria', source='stamen', zoom=6,
		maptype='toner-lines' )
	save(ngaLines, file=paste0(pathData, 'nigeriaStamenLines.rda'))
} else { load(paste0(pathData, 'nigeriaStamenLines.rda')) }


# org features in acled data
nData$id = as.numeric(as.character(unique(nigShape$id))	)
nData$postBH = ifelse(nData$YEAR>=2009,1,0)
nData$invBoko = apply(nData[,c('a1','a2')], 1, function(x){ ifelse('Boko Haram' %in% x,'Yes','No')  })
nData$invBoko = factor(nData$invBoko, levels=c('Yes','No'))
nData$yearLab = ifelse(nData$YEAR>=2009, nData$YEAR, paste0(nData$YEAR, ' (Pre Boko Haram)'))
nData$yearLab = factor(nData$yearLab, levels=sort(unique(nData$yearLab)))
cols = brewer.pal(3,'Set1')[1:2] ; names(cols)=c('Yes',"No")

# just focus on civ vio
nDataCiv = nData[which(nData$EVENT_TYPE=='Violence against civilians'),]
# nDataCiv = nData

# nDataCiv$dyad = paste(nDataCiv$a1, nDataCiv$a2, sep='_')
# tmp = nDataCiv[,c('a1','a2','YEAR','yearLab','dyad')]
# toDrop=c(
# 	'Civilians (Nigeria)', 'Rioters (Nigeria)', 
# 	'Civilians (International)',
# 	'Military Forces of Nigeria', 'Police Forces of Nigeria', 
# 	NA
# 	)
# load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
# aKey = getNameKey(yList) ; aKey2 = aKey[-grep('Forces of Nigeria',aKey$dirty),]
# tmp = tmp[which(tmp$a1 %in% aKey2$dirty),]
# tmp = tmp[which(tmp$a2 %in% aKey2$dirty),]

# cbind(sort(table(tmp$dyad), decreasing=TRUE)[1:10])

################
# avg location based on where events took place
actors = unique(c(nDataCiv$a1,nDataCiv$a2))
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
yrs = char(2000:2016) ; yList = yList[yrs]
aKey = getNameKey(yList) ; aKey2 = aKey[-grep('Forces of Nigeria',aKey$dirty),]
aKey = aKey[c(5, 10, 23, 24, 28),]
aKey2 = aKey

loadPkg('abind') ; rownames(nDataCiv) = NULL
aData = data.frame( abind(
	nDataCiv[,c('a1','LATITUDE','LONGITUDE','YEAR')],
	nDataCiv[,c('a2','LATITUDE','LONGITUDE','YEAR')],
	along=1 ) )
aData = aData[which(aData$a2 %in% aKey$dirty),]
aData$name = aKey$clean[match(aData$a2,aKey$dirty)]
for(v in c('LATITUDE','LONGITUDE')){ aData[,v] = num(aData[,v])}

aData$YEAR = num(aData$YEAR)
yKey = data.frame(t=2001:2016, lab=cut(2001:2016,4))
aData$pd = yKey$lab[match(aData$YEAR,yKey$t)]
aData$pd = aData$YEAR

sdX = function(x){ ifelse(length(x)==1,.0000000001,sd(x))  }
aGeo = aData[,-which(names(aData) %in% c('a2','YEAR'))] %>%
	group_by(name,pd) %>% summarise_each(funs(mean,sdX,min,max)) %>% data.frame()
toAdd = setdiff(names(aGeo),names(aData))
for(v in toAdd){
	aData$tmp = aGeo[,v][match(paste0(aData$name,aData$pd),paste0(aGeo$name,aGeo$pd))]
	names(aData)[ncol(aData)]=v }

aData$id = as.numeric(as.character(unique(nigShape$id))	)
aGeo$id = as.numeric(as.character(unique(nigShape$id)) )

aData = aData[which(aData$name %in% aKey2$clean),]
aGeo = aGeo[which(aGeo$name %in% aKey2$clean),]

# aGeo = aGeo[aGeo$name=='Vigilante\nMilitia',]
# aData = aData[aData$name=='Vigilante\nMilitia',]
ggNigConfMapActor = ggplot(aGeo, aes(map_id = id, x=LONGITUDE_mean,y=LATITUDE_mean)) + 
	geom_map( map=nigShape, fill='transparent', linetype=1, colour='grey80') +
	geom_point(alpha=.7) + 
	geom_point(data=aData, aes(x=LONGITUDE, y=LATITUDE,color=name,shape=name),alpha=.6) + 
	geom_text_repel(
		aes(label=name), 
		size=2
		# , family='Source Sans Pro Bold'
		) +

	facet_wrap(~pd, nrow=4,ncol=4) + 
	xlab('') + ylab('') + 
	labs(color='', shape='') + 
	theme(
		legend.position = 'bottom',
		panel.border=element_blank(),
		axis.ticks=element_blank()
		)
ggNigConfMapActor
fName = paste0(pathGraphics,'nigConfMapActor_onlyVioAgainstCiv_topthree_wgov.pdf')
ggsave(ggNigConfMapActor, file=fName, width=10, height=8, device=cairo_pdf)
system( paste('pdfcrop', fName, fName, sep=' ') )	
################