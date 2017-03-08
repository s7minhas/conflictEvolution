################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }

# add add'l libs
loadPkg('abind')
################

################
# load data
load(paste0(pathData,"exoVars.rda"))
################

################
# add election counter as dyad var
elecs = char(c(2003,2007,2011,2015))
for(t in names(xDyadL)){
	elec = matrix(1, nrow=nrow(xDyadL[[t]]), ncol=ncol(xDyadL[[t]]),
		dimnames=dimnames(xDyadL[[t]])[1:2])
	if(t %in% elecs){ xDyadL[[t]] = abind(xDyadL[[t]], elecYear=elec, along=3) }
	if(!t %in% elecs){ xDyadL[[t]] = abind(xDyadL[[t]], elecYear=elec*0, along=3) }
}
################

################
# add sum of contiguous country conflict
loadPkg('dplyr')
spatConf = read.csv(paste0(pathData, 'nigeria_neighbor_conflict.csv')) %>%
	group_by(YEAR) %>% summarise( count = n() )
for(t in names(xDyadL)){
	ngbrConfCount = matrix(spatConf$count[spatConf$YEAR==num(t)],
		nrow=nrow(xDyadL[[t]]), ncol=ncol(xDyadL[[t]]),
		dimnames=dimnames(xDyadL[[t]])[1:2])
	xDyadL[[t]] = abind(xDyadL[[t]], ngbrConfCount=ngbrConfCount, along=3)
}
################

################
# save
save(xDyadL, xNodeL, file = paste0(pathData,"exoVars.rda"))
################