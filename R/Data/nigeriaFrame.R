################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
#################

battleData<-read.csv(paste0(pathData,"ACLED-Version-5-All-Africa-1997-2014_battles.csv"), na.strings="", stringsAsFactors=FALSE)
nigeria<-subset(battleData, COUNTRY=="Nigeria")

#Nigerian Sample Frame
#clean
d<-nigeria$ACTOR1[grep("Unidentified", nigeria$ACTOR1)]
ndata = nigeria[!nigeria$ACTOR1 %in% d,]
gsub("'", "", ndata$ACTOR1)
ndata$a1=char(ndata$ACTOR1) %>% trim()
ndata$a2=char(ndata$ACTOR2) %>% trim()

#write.csv(ndata, file=paste0(pathData, "nigeriaClean.csv"))
#ndata = read.csv(paste0(pathData, "nigericaClean.csv"))

# flip over dataset
orig = ndata
revOrig = orig ; revOrig$a2 = orig$a1 ; revOrig$a1 = orig$a2
tmp = rbind(orig, revOrig)
yrs=seq(min(ndata$YEAR), max(ndata$YEAR), by=1)

loadPkg('doBy')
actorDates = summaryBy(YEAR ~ a1, data=tmp, FUN=c(min, max))

# should we only look at actors involved for at least a span of five years?
actorDates$yrsActive = actorDates$YEAR.max - actorDates$YEAR.min

# get rid of some remaining Unidentified actors
actorDates = actorDates[!grepl('Unidentified', actorDates$a1),]

# list of actors by year
actorsT = lapply( yrs, function(t){
  actors = NULL
  for( ii in 1:nrow(actorDates)){
     if( t %in% actorDates$YEAR.min[ii]:actorDates$YEAR.max[ii] ) { 
      actors = append(actors, actorDates$a1[[ii]]) } }
  return(actors)
}) ; names(actorsT) = yrs

# adj mats
ndata$dv = 1 ; yVar = 'dv'
yList = lapply(1997:2014, function(ii){ # 2006 is when we have +100 actors
  actorSlice = actorsT[[char(ii)]]
  slice = ndata[ which( 
      ndata$YEAR==ii & 
      ndata$a1 %in% actorSlice &
      ndata$a2 %in% actorSlice
      ), c('a1', 'a2', yVar) ]
  adjMat = matrix(0, 
    nrow=length(actorSlice), ncol=length(actorSlice),
    dimnames=list(actorSlice,actorSlice) )
  for(r in 1:nrow(slice)){ adjMat[slice$a1[r],slice$a2[r]]=1  }
  return(adjMat)
}) ; names(yList) = yrs