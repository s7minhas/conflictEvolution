################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
source(paste0(fPth, 'actorInfo.R'))
################

################
# load data
load(paste0(pathData, "nData.rda"))
nData = nData[
	which(nData$YEAR %in% 2001:2016),
	c('YEAR','a1','a2','LATITUDE','LONGITUDE')
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
ngaLines = ggmap::get_map(location='Nigeria', source='stamen', zoom=6,
	maptype='toner-lines' )

# org features in acled data
nData$id = as.numeric(as.character(unique(nigShape$id))	)
nData$postBH = ifelse(nData$YEAR>=2009,1,0)
nData$invBoko = apply(nData[,c('a1','a2')], 1, function(x){ ifelse('Boko Haram' %in% x,'Yes','No')  })
nData$invBoko = factor(nData$invBoko, levels=c('Yes','No'))

# viz
ggNigConfMap = ggplot(nData, aes(map_id = id)) + 
	geom_map( map=nigShape, fill='white', linetype=1, colour='grey50') +
	inset_ggmap(ngaLines) +
	expand_limits(x = nigShape$long, y = nigShape$lat) +
	geom_point(aes(x=LONGITUDE,y=LATITUDE, color=factor(invBoko)),alpha=.7) + 
	facet_wrap(~YEAR, nrow=4, ncol=4) + 
	xlab('') + ylab('') + 
	labs(color='Confict Involving Boko Haram?') + 
	theme(
		legend.position = 'bottom',
		panel.border=element_blank(),
		panel.grid=element_blank(),
		axis.ticks=element_blank(),
		axis.text=element_blank(),
		legend.text=element_text(family='Source Sans Pro Bold'),
		legend.title=element_text(family='Source Sans Pro Bold'),
		strip.text.x = element_text(color='white',family="Source Sans Pro Bold"),
		strip.background = element_rect(fill = "#525252", color='#525252')		
		)
fName = paste0(pathGraphics,'nigConfMap.pdf')
ggsave(ggNigConfMap, file=fName, width=8, height=8, device=cairo_pdf)
system( paste('pdfcrop', fName, fName, sep=' ') )
################