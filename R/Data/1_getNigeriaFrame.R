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
nData<-read.csv(
  paste0(pathData,"ACLED-Version-7-Nigeria-1997-2016_csv_dyadic-file.csv"), 
  na.strings="", stringsAsFactors=FALSE)
#################

#################
# clean actor names
nData$a1 = trim(nData$ACTOR1) ; nData$a2 = trim(nData$ACTOR2)
nData$aa1 = trim(nData$ALLY_ACTOR_1) ; nData$aa2 = trim(nData$ALLY_ACTOR_2)
ids = c('a1','a2','aa1','aa2')

# replace military forces of nigeria and policies forces of nigiera year ids
for(id in ids){
	nData[,id][which(grepl('Military Forces of Nigeria',nData[,id]))] = 'Military Forces of Nigeria' }
for(id in ids){
	nData[,id][which(grepl('Police Forces of Nigeria',nData[,id]))] = 'Police Forces of Nigeria' }

# nscdc paramilitary arm of nigeria
for(id in ids){
	nData[,id][nData[,id]=='NSCDC: Nigeria Security and Civil Defence Corps'] = 'Military Forces of Nigeria' }

# create master Oodua Peoples Congress category
for(id in ids){
	nData[,id][nData[,id]=="Mutiny of OPC: Oodua Peoples Congress"] = "OPC: Oodua Peoples Congress" }

# create master boko haram category
for(id in ids){
	nData[,id][which(grepl('Boko Haram',nData[,id]))] = 'Boko Haram' }

# remove unidentified groups
for(id in ids[1:2]){ nData = nData[which(!grepl('Unidentified', nData[,id])),] }
#################

#################
# save
save(nData, file=paste0(pathData, 'nData.rda'))
#################