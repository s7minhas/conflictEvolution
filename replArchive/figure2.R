################
# workspace
source('setup.R')
source('actorInfo.R')
loadPkg(c('dplyr','abind','ggrepel','fossil'))
################

################
# load data
load("nData.rda")
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
load('nigeriaStamenLines.rda')

# org features in acled data
nData$id = as.numeric(as.character(unique(nigShape$id))	)
nData$postBH = ifelse(nData$YEAR>=2009,1,0)
nData$invBoko = apply(nData[,c('a1','a2')], 1, function(x){ ifelse('Boko Haram' %in% x,'Yes','No')  })
nData$invBoko = factor(nData$invBoko, levels=c('Yes','No'))
nData$yearLab = ifelse(nData$YEAR>=2009, nData$YEAR, paste0(nData$YEAR, ' (Pre Boko Haram)'))
nData$yearLab = factor(nData$yearLab, levels=sort(unique(nData$yearLab)))
cols = brewer.pal(3,'Set1')[1:2] ; names(cols)=c('Yes',"No")

# viz
ggNigConfMap = ggplot(nData, aes(map_id = id, x=LONGITUDE,y=LATITUDE)) + 
	geom_map( map=nigShape, fill='white', linetype=1, colour='grey30') +
	inset_ggmap(ngaLines) +
	geom_point(aes(color=factor(invBoko)),alpha=.7) + 
	facet_wrap(~yearLab, nrow=4, ncol=4) + 
	xlab('') + ylab('') + 
	scale_color_manual('Confict Involving Boko Haram?', values=cols) + 
	theme(
		legend.position = 'bottom',
		panel.border=element_blank(),
		panel.grid=element_blank(),
		axis.ticks=element_blank(),
		axis.text=element_blank(),
		strip.text.x = element_text(color='white'),
		strip.background = element_rect(fill = "#525252", color='#525252')		
		)
fName = paste0('figure2.pdf') ; h=8 ; w=8
ggsave(ggNigConfMap, file=fName, width=w, height=h)
################