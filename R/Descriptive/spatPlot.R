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
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
yrs = char(2000:2016) ; yList = yList[yrs]
# pull out array
yArr = listToArray(actors=getActor(yList), Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
################


################
con <- url("http://biogeo.ucdavis.edu/data/gadm2.8/rds/NGA_adm1.rds")
print(load(con)) ; close (con)
nigeria_map.ff <- fortify(gadm)

# Extract polygon corners and merge with shapefile data
gadm@data$id <- rownames(gadm@data)
nigeria_map.df <- merge(gadm@data, nigeria_map.ff, by = "id", all.y = TRUE)

#Build up the plot
nigeriaMap = ggplot() + 
	geom_path(data = nigeria_map.df,
		aes(x = long, y = lat, group = group)) + 
	theme_bw() +
	theme(
		panel.border = element_blank(),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank()
		)

loadPkg('maps')
nMapData=map(regions='Nigeria', fill=TRUE, add=TRUE, exact=TRUE, col='grey')
################