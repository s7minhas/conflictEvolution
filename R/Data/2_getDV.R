################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
  source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
  source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
  source('~/Documents/conflictEvolution/R/setup.R')  }
#################

#################
# read in data
load(paste0(pathData, 'nData.rda')) # loads nData object
#################

#################
# subset to battles
battles = c("Battle-Government regains territory", 
  "Battle-No change of territory", "Battle-Non-state actor overtakes territory")
nData = nData[which(nData$EVENT_TYPE %in% battles),]
#################

#################
# get dates actors were active

# flip over dataset to get actor dates
orig = nData ; revOrig = orig
revOrig$a2 = orig$a1 ; revOrig$a1 = orig$a2
tmp = rbind(orig, revOrig)
yrs=seq(min(nData$YEAR), max(nData$YEAR), by=1)
loadPkg('doBy') ; actorDates = doBy::summaryBy(YEAR ~ a1, data=tmp, FUN=c(min, max))
actorDates$yrsActive = actorDates$YEAR.max - actorDates$YEAR.min # length of years active
# actorDates = actorDates[actorDates$yrsActive > 2,] # only keep actors involved in 3 yrs of conflict
actorDates = actorDates[actorDates$yrsActive > 4,] 

# list of actors by year
actorsT = lapply( yrs, function(t){
  actors = NULL
  for( ii in 1:nrow(actorDates)){
     if( t %in% actorDates$YEAR.min[ii]:actorDates$YEAR.max[ii] ) { 
      actors = append(actors, actorDates$a1[[ii]]) } }
  return(actors)
}) ; names(actorsT) = yrs
#################

#################
# create list of adj mats
# adj mats
nData$dv = 1 ; yVar = 'dv'
yList = lapply(1997:2016, function(ii){ 
  actorSlice = actorsT[[char(ii)]]
  slice = nData[ which( 
      nData$YEAR==ii & 
      nData$a1 %in% actorSlice &
      nData$a2 %in% actorSlice
      ), c('a1', 'a2', yVar) ]
  adjMat = matrix(0, 
    nrow=length(actorSlice), ncol=length(actorSlice),
    dimnames=list(actorSlice,actorSlice) )
  for(r in 1:nrow(slice)){ adjMat[slice$a1[r],slice$a2[r]]=1  }
  return(adjMat)
}) ; names(yList) = yrs
#################

#################
# create list of adj mats fatalities>0
nData$dv = 0 ; yVar = 'dv'
nData$dv[nData$FATALITIES>0]=1
yListFatal = lapply(1997:2016, function(ii){ 
  actorSlice = actorsT[[char(ii)]]
  slice = nData[ which( 
      nData$YEAR==ii & 
      nData$a1 %in% actorSlice &
      nData$a2 %in% actorSlice
      ), c('a1', 'a2', yVar) ]
  adjMat = matrix(0, 
    nrow=length(actorSlice), ncol=length(actorSlice),
    dimnames=list(actorSlice,actorSlice) )
  for(r in 1:nrow(slice)){ adjMat[slice$a1[r],slice$a2[r]]=1  }
  return(adjMat)
}) ; names(yListFatal) = yrs
#################

#################
# cleanup some irrelev or no interaction actors
other=c('NURTW: National Union of Road Transport Workers',
  'RTEAN: Road Transport Employers Association of Nigeria',
  'Private Security Forces (Nigeria)',"Viking 22 Student Militia",
  "Aguleri Ethnic Militia (Nigeria)", "Black Axe Student Militia", # 0 interactions
  "Egbas Ethnic Militia (Nigeria)", "Shuwa Ethnic Militia (Nigeria)", # 0 interactions
  "Ife Ethnic Militia (Nigeria)", "Modakeke Ethnic Militia (Nigeria)", #drops out pre 2000
  "Dadiya Ethnic Militia (Nigeria)"
  )
polParties=c(
  "PDP: People's Democratic Party", "PDP: Peoples Democratic Party",
  "ANPP: All Nigeria People's Party", "ANPP: All Nigeria Peoples Party",
  "AP: Action Party", "AC: Action Congress", "AD: Alliance for Democracy",
  "APC: All Progressives Congress", "APP: All Peoples Party",
  "CPC: Congress for Progressive Change"
  )
drop = c(other, polParties)
yList=lapply(yList,
  function(y){ toKeep = setdiff(rownames(y), drop); return(y[toKeep,toKeep]) })
yListFatal=lapply(yListFatal,
  function(y){ toKeep = setdiff(rownames(y), drop); return(y[toKeep,toKeep]) })
#################

#################
# save
save(yList, yListFatal,
  file=paste0(pathData,"nigeriaMatList_acled_v7.rda") # label with acled number
  )
#################