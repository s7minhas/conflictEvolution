
################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
#################

battleData<-read.csv(paste0(pathData,"ACLED-Version-5-All-Africa-1997-2014_battles.csv"), na.strings="", stringsAsFactors=FALSE)
nigeria<-subset(battleData, COUNTRY=="Nigeria")
badnames = c("Viking 22 Student Militia")
ndata = ndata[(!ndata$a1 %in% badnames) & (!ndata$a2 %in% badnames),]
#Nigerian Sample Frame
#clean
d<-nigeria$ACTOR1[grep("Unidentified", nigeria$ACTOR1)]
ndata = nigeria[!nigeria$ACTOR1 %in% d,]

gsub("'", "", ndata$ACTOR1)
ndata$a1=char(ndata$ACTOR1) %>% trim()
ndata$a2=char(ndata$ACTOR2) %>% trim()

ndata[ndata$a1=="Military Forces of Nigeria (1993-1998)",]$a1<-"Military Forces of Nigeria"
ndata[ndata$a1=="Military Forces of Nigeria (1993-1999)",]$a1<-"Military Forces of Nigeria"
ndata[ndata$a2=="Military Forces of Nigeria (1993-1999)",]$a2<-"Military Forces of Nigeria"
ndata[ndata$a1=="Military Forces of Nigeria (1999-2007)",]$a1<-"Military Forces of Nigeria"
ndata[ndata$a2=="Military Forces of Nigeria (1999-2007)",]$a2<-"Military Forces of Nigeria"
ndata[ndata$a1=="Military Forces of Nigeria (2007-2010)",]$a1<-"Military Forces of Nigeria"
ndata[ndata$a1=="Military Forces of Nigeria (2010-)",$a1<-"Military Forces of Nigeria"
ndata[ndata$a1=="Military Forces of Nigeria (2010-)",]$a1<-"Military Forces of Nigeria"
ndata[ndata$a1=="Military Forces of Nigeria (1999-2007) Joint Task Force",]$a1<-"Military Forces of Nigeria"
ndata[ndata$a1=="Military Forces of Nigeria (2007-2010) Joint Task Force",]$a1<-"Military Forces of Nigeria"
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
actorDates = actorDates[actorDates$yrsActive > 3,]
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
yList = lapply(1997:2014, function(ii){ 
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

save(yList, file=(paste0(pathData,"nigeriaMatList.rda"))
#graph
library(igraph)

#plot style from igraph- easier to interpret overtime
#1997
diag(yList[[1]])<-0
graph1997<-graph.adjacency(as.matrix(yList[[1]]), mode="directed", weighted=TRUE)
ind<-c(1,1,1, 2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 2,1,1,1,1,2,1,1)
color<-cbind(names,ind)
names<-dimnames(yList[[1]])
color<-cbind(names[[1]],ind)

graph1997$type=color[,2]
V(graph1997)$color=graph1997$type
V(graph1997)$color=gsub("c", "aquamarine3", V(graph1997)$color)
V(graph1997)$color=gsub("gov", "burlywood", V(graph1997)$color)
V(graph1997)$size=degree(graph1997)+1
V(graph1997)$label.cex=.6

plot(graph1997, vertex.size=3, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.2, edge.width=E(graph1997)$weight,
     vertex.label.family="Helvetica", edge.arrow.size=.5,
     layout=layout.fruchterman.reingold(graph1997, niter=10000))
text(1.2,1.2, "1997", font=2)

plot.igraph(graph1997,vertex.size=5,vertex.label=NA,
     edge.curved=seq(.1, .1),edge.arrow.size=.5,
    layout=layout.fruchterman.reingold(graph1997, niter=10000))

#1998
diag(yList[[2]])<-0
graph1998<-graph.adjacency(as.matrix(yList[[2]]), mode="directed", weighted=TRUE)
ind<-c(3,3,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,3,2,1,1,1,1,1)
names<-dimnames(yList[[2]])
color<-cbind(names[[2]],ind)

graph1998$type=color[,2]
V(graph1998)$color=graph1998$type
V(graph1998)$color=gsub("c", "aquamarine3", V(graph1998)$color)
V(graph1998)$color=gsub("mil", "burlywood", V(graph1998)$color)
V(graph1998)$color=gsub("party", "green", V(graph1998)$color)

V(graph1998)$size=degree(graph1998)+1
V(graph1998)$label.cex=.6

plot(graph1998, vertex.size=3, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.2, edge.width=E(graph1998)$weight,
     vertex.label.family="Helvetica", edge.arrow.size=.5,
     layout=layout.fruchterman.reingold(graph1998, niter=10000))
text(1.2,1.2, "1998", font=2)

