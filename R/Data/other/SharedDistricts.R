################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
#################

battleData<-read.csv(paste0(pathData,"ACLED-Version-5-All-Africa-1997-2014_battles.csv"), na.strings="", stringsAsFactors=FALSE)
nigeria<-subset(battleData, COUNTRY=="Nigeria")
badnames = c("Viking 22 Student Militia")
#Nigerian Sample Frame
#clean
d<-nigeria$ACTOR1[grep("Unidentified", nigeria$ACTOR1)]
ndata = nigeria[!nigeria$ACTOR1 %in% d,]

ndata$a1=char(ndata$ACTOR1) %>% trim()
ndata$a2=char(ndata$ACTOR2) %>% trim()

ndata[ndata$a1=="Military Forces of Nigeria (1993-1998)",]$a1<-"Military Forces of Nigeria"
ndata[ndata$a1=="Military Forces of Nigeria (1993-1999)",]$a1<-"Military Forces of Nigeria"
ndata[ndata$a2=="Military Forces of Nigeria (1993-1999)",]$a2<-"Military Forces of Nigeria"
ndata[ndata$a1=="Military Forces of Nigeria (1999-2007)",]$a1<-"Military Forces of Nigeria"
ndata[ndata$a2=="Military Forces of Nigeria (1999-2007)",]$a2<-"Military Forces of Nigeria"
ndata[ndata$a1=="Military Forces of Nigeria (2007-2010)",]$a1<-"Military Forces of Nigeria"
ndata[ndata$a1=="Military Forces of Nigeria (2010-)",]$a1<-"Military Forces of Nigeria"
ndata[ndata$a1=="Military Forces of Nigeria (1999-2007) Joint Task Force",]$a1<-"Military Forces of Nigeria"
ndata[ndata$a2=="Military Forces of Nigeria (2007-2010) Joint Task Force",]$a2<-"Military Forces of Nigeria"
#ndata[ndata$a1=="Police Forces of Nigeria (1993-1998)",]$a1<-"Police Forces of Nigeria"
ndata[ndata$a2=="Police Forces of Nigeria (1993-1998)",]$a2<-"Police Forces of Nigeria"
ndata[ndata$a2=="Police Forces of Nigeria (1998-1999)",]$a2<-"Police Forces of Nigeria"
ndata[ndata$a1=="Police Forces of Nigeria (1999-2007)",]$a1<-"Police Forces of Nigeria"
ndata[ndata$a2=="Police Forces of Nigeria (1999-2007)",]$a2<-"Police Forces of Nigeria"
ndata[ndata$a1=="Police Forces of Nigeria (2007-2010)",]$a1<-"Police Forces of Nigeria"
ndata[ndata$a2=="Police Forces of Nigeria (2007-2010)",]$a2<-"Police Forces of Nigeria"
ndata[ndata$a1=="Police Forces of Nigeria (2010-)",]$a1<-"Police Forces of Nigeria"
ndata[ndata$a2=="Police Forces of Nigeria (2010-)",]$a2<-"Police Forces of Nigeria"
ndata[ndata$a1=="AD: Alliance for Democracy",]$a1<-"AD: Alliance For Democracy"

# some extra dupe replacements
toReplace = c(
  "Military Forces of Niger (2011-)",
  "Military Forces of Nigeria (2007-2010)",
  "Military Forces of Nigeria (2007-2010) Joint Task Force",
  "Military Forces of Nigeria (2010-)"
)
for(label in toReplace){
  ndata$a1[ndata$a1==label] = "Military Forces of Nigeria"
  ndata$a2[ndata$a2==label] = "Military Forces of Nigeria"
}

#write.csv(ndata, file=paste0(pathData, "nigeriaClean.csv"))
#ndata = read.csv(paste0(pathData, "nigericaClean.csv"))

# flip over dataset
orig = ndata
revOrig = orig ; revOrig$a2 = orig$a1 ; revOrig$a1 = orig$a2
tmp = rbind(orig, revOrig)
yrs=seq(min(ndata$YEAR), max(ndata$YEAR), by=1)

loadPkg('doBy')
actorDates = summaryBy(YEAR ~ a1, data=tmp, FUN=c(min, max))

# length of years active
actorDates$yrsActive = actorDates$YEAR.max - actorDates$YEAR.min

# get rid of some remaining Unidentified actors
actorDates = actorDates[!grepl('Unidentified', actorDates$a1),]
actorDates = actorDates[actorDates$yrsActive > 2,]
# list of actors by year
actorsT = lapply( yrs, function(t){
  actors = NULL
  for( ii in 1:nrow(actorDates)){
    if( t %in% actorDates$YEAR.min[ii]:actorDates$YEAR.max[ii] ) { 
      actors = append(actors, actorDates$a1[[ii]]) } }
  return(actors)
}) ; names(actorsT) = yrs



shared.dist = function(a1, a2, year){
  if(a1 %in% c("Military Forces of Nigeria", "Police Forces of Nigeria") | a2 %in% c("Military Forces of Nigeria", "Police Forces of Nigeria")){
    return(1)
  }
  a = unique(ndata$ADMIN1[(ndata$a1 == a1 | ndata$a2 == a1) & ndata$YEAR == year])
  b = unique(ndata$ADMIN1[(ndata$a1 == a2 | ndata$a2 == a2) & ndata$YEAR == year])
  if(length(a) == 0 | length(b)== 0){return(0)}
  return(max(a %in% b))
  }

shared.district = lapply(1:length(actorsT), function(t) 
  sapply(actorsT[[t]], function(j) sapply(actorsT[[t]], function(i) shared.dist(i,j, t + 1996))))


shared.district.lag = lapply(2:length(actorsT), function(t) 
  sapply(actorsT[[t]], function(j) sapply(actorsT[[t]], function(i) shared.dist(i,j, t + 1995))))


save(shared.district, shared.district.lag, file = paste0(pathData, "shareddistrict.rda"))
