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
vioContraCiv$YEAR = vioContraCiv$YEAR + 1 # use lagged x

rioData = nData[nData$EVENT_TYPE=='Riots/Protests',c('x','YEAR','a1','a2','aa2')]
rioData$protests = rioData$a1 == "Protesters (Nigeria)"
rioData$riots = rioData$a1 == "Rioters (Nigeria)"

tmp1 = na.omit(rioData[,-5]) ; tmp2 = na.omit(rioData[,-4])

names(tmp2) = names(tmp1) ; rioData=rbind(tmp1,tmp2)
riotData = rioData[rioData$riots == 1,]
protData = rioData[rioData$protests == 1,]

rioContraActor = summaryBy(x~YEAR+a2,
	data=riotData,
	FUN=sum)
rioContraActor$YEAR = rioContraActor$YEAR + 1 # use lagged x

protContraActor = summaryBy(x~YEAR+a2,
                            data=protData,
                            FUN=sum)
protContraActor$YEAR = protContraActor$YEAR + 1 # use lagged x
#################

#################
# chop off first element in Y due to lag
yList = yList[-1]
#################

#################
# create list of nodal covariates for amen
xNodeL = lapply(1:length(yList), function(t){
	# subset y to pull in relev info
	y = yList[[t]]
	actors = rownames(y) ; n=nrow(y)

	# create blank nodeMat
	nodeMat = matrix(0, nrow=n, ncol=4, 
		dimnames=list(actors, 
			c('vioCivEvents','vioCivFatals', 'riotsAgainst', 'protestsAgainst')))
	
	# add in civ vio data
	civSlice=vioContraCiv[vioContraCiv$YEAR==num(names(yList)[t]),]
	civSlice=civSlice[which(civSlice$a1 %in% rownames(nodeMat)),]
	nodeMat[civSlice$a1,'vioCivEvents'] = civSlice$x.sum
	nodeMat[civSlice$a1,'vioCivFatals'] = civSlice$FATALITIES.sum

	# add in riots data
	rioSlice=rioContraActor[rioContraActor$YEAR==num(names(yList)[t]),]
	rioSlice=rioSlice[which(rioSlice$a2 %in% rownames(nodeMat)),]
	nodeMat[rioSlice$a2,'riotsAgainst'] = rioSlice$x.sum

	protSlice=protContraActor[protContraActor$YEAR==num(names(yList)[t]),]
	protSlice=protSlice[which(protSlice$a2 %in% rownames(nodeMat)),]
	nodeMat[protSlice$a2,'protestsAgainst'] = protSlice$x.sum
	
	# 
	return(nodeMat)
}) ; names(xNodeL) = names(yList)
#################

#################
# designate nigeria gov actors 
actors=unique(unlist(lapply(yList, rownames)))
govActors=c('Military Forces of Nigeria','Police Forces of Nigeria')

## no lags necessary here since each of these vars is time invariant
# Dyadic covar
xDyadL = lapply(1:length(yList), function(t){
	actors = rownames( yList[[t]] )
	yr = num(names(yList)[t])
	xArr = array(0,dim=c(length(actors),length(actors),2),dimnames=list(actors,actors,c('govActor','postBoko')))
	xArr[which(rownames(xArr) %in% govActors),which(colnames(xArr) %in% govActors),'govActor'] = 1
	if(yr>2008){xArr[,,'postBoko']=1} # boko enters network in 2009
	for(p in 1:dim(xArr)[3]){ diag(xArr[,,p])=NA }
	return(xArr)
}) ; names(xDyadL) = names(yList)
#################

#################
# save
save(xNodeL, xDyadL, file=paste0(pathData,'exoVars.rda'))
#################