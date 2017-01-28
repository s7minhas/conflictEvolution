################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
#################

#################
# load data
load(paste0(pathData, 'nData.rda')) # loads nData object
load(paste0(pathData,"nigeriaMatList_acled_v7.rda")) # adds object named yList
#################

#################
# count up number of events actors committed against
# violent civilians by year
loadPkg('doBy') ; nData$x = 1
vioContraCiv = summaryBy(x+FATALITIES~YEAR+a1,
	data=nData[nData$EVENT_TYPE=='Violence against civilians',],
	FUN=sum)

rioData = nData[nData$EVENT_TYPE=='Riots/Protests',c('x','YEAR','a2','aa2')]
tmp1 = na.omit(rioData[,-4]) ; tmp2 = na.omit(rioData[,-3])
names(tmp2) = names(tmp1) ; rioData=rbind(tmp1,tmp2)
rioContraActor = summaryBy(x~YEAR+a2,
	data=rioData,
	FUN=sum)
#################

#################
# create list of nodal covariates for amen
xNodeL = lapply(1:length(yList), function(t){
	# subset y to pull in relev info
	y = yList[[t]]
	# create blank nodeMat
	nodeMat = matrix(0, nrow=nrow(y), ncol=3, 
		dimnames=list(rownames(y), 
			c('vioCivEvents','vioCivFatals', 'riotsAgainst')))
	
	# add in civ vio data
	civSlice=vioContraCiv[vioContraCiv$YEAR==num(names(yList)[t]),]
	civSlice=civSlice[which(civSlice$a1 %in% rownames(nodeMat)),]
	nodeMat[civSlice$a1,'vioCivEvents'] = civSlice$x.sum
	nodeMat[civSlice$a1,'vioCivFatals'] = civSlice$FATALITIES.sum

	# add in riots data
	rioSlice=rioContraActor[rioContraActor$YEAR==num(names(yList)[t]),]
	rioSlice=rioSlice[which(rioSlice$a2 %in% rownames(nodeMat)),]
	nodeMat[rioSlice$a2,'riotsAgainst'] = rioSlice$x.sum

	# 
	return(nodeMat)
}) ; names(xNodeL) = yList
#################

#################
# save
save(xNodeL, file=paste0(pathData,'nodalVars_fromACLED.rda'))
#################