#################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
#################

#################
# Some parameters for network data
timeLevel = 'quarterly' # yearly, quarterly
#################

#################
# Load data
cleanData = read.csv(paste0(pathData, "mexicoVioStoriesFinal.csv")) #1051 obs
# Drop desig obs in cleanData
cleanData = cleanData[which(cleanData$drop1==0 & cleanData$directional==1 ),]
# Add var to count
cleanData$event = 1
# Subset to relev year
cleanData = cleanData[cleanData$year>=2005,]
#################

#################
# Create date var
cleanData$date = char(cleanData$event_date) %>% gsub('/', '-', ., fixed=TRUE)
cleanData$month = strsplit(cleanData$date, '-') %>% lapply(., function(x){ x[2]  }) %>% unlist() %>% num()
cleanData = cleanData[!is.na(cleanData$month),] # drop one obs without an event date
cleanData$mYr = as.Date(paste(1, cleanData$month, cleanData$year, sep='/'), '%d/%m/%Y')
cleanData$qtr = 1
cleanData$qtr[cleanData$month>3 & cleanData$month<7] = 2
cleanData$qtr[cleanData$month>6 & cleanData$month<10] = 3
cleanData$qtr[cleanData$month>9] = 4
cleanData$qYr = as.Date(paste(1, cleanData$qtr, cleanData$year, sep='/'), '%d/%m/%Y')

# Choose a date var
if(timeLevel == 'yearly'){ cleanData$time = cleanData$year }
if(timeLevel == 'quarterly'){ cleanData$time = cleanData$qYr } 
#################

#################
# Clean up sender and target vars
actorIDs=c( "senderGroup1", "senderGroup2", "senderGroup3", "TargetGroup1", "TargetGroup2", "TargetGroup3" )
keep = c( actorIDs, "mexicanState", 'time' )
cleanData = cleanData[,c(keep)]
ugh = c('n/a','na', '')
for(var in actorIDs){
	cleanData[,var] = char( cleanData[,var]  )
	cleanData[,var] = trim(cleanData[,var])
	cleanData[,var] = tolower(cleanData[,var])
	cleanData[,var][ which(cleanData[,var] %in% ugh)  ] = NA	
}

# clean up actor names in cleanData
panel = read.csv(paste0(pathData,"mexicanActorListMonth.csv"))
panel = panel[!is.na(panel$startYear),]
for(var in actorIDs){ cleanData[,var] = panel$lab[ match( cleanData[,var], panel$labOld ) ] }

# Determine number of cases in which all sender or all target vars are NA
cleanData$senCnt = apply(cleanData[,grep('sender',actorIDs)], 1, function(x){ sum(!is.na(x)) } )
cleanData$tarCnt = apply(cleanData[,grep('Target',actorIDs)], 1, function(x){ sum(!is.na(x)) } )
table(cleanData$senCnt) ; table(cleanData$tarCnt)

# Drop 6 sender all NA cases and 1 target all NA cases
cleanData = cleanData[which(cleanData$senCnt != 0), ]
cleanData = cleanData[which(cleanData$tarCnt != 0), ]
#################

head(cleanData)
govActors = c('federal','state','municipal')
cleanData$tmp = apply(cleanData[,actorIDs], 1, function(x){
	ugh = x[!is.na(x)]
	ret=ifelse(sum(govActors %in% ugh)>0, 1, 0)
	return(ret)
	})

#################
# Finalize actor list
pds = cleanData$time %>% unique() %>% sort()

if(timeLevel=='quarterly'){
	panel$qtr = 1
	panel$qtr[panel$startMonth=='September'] = 3
	panel$time = paste( panel$qtr, panel$startYear, sep='_')
	panel$qYr = as.Date(paste(1, panel$qtr, panel$startYear, sep='/'), '%d/%m/%Y')
	actorList = lapply(pds, function(t){ unique( char( panel[which(panel$qYr<=t),'lab'] ) ) }) }

if(timeLevel=='yearly'){
	actorList = lapply(pds, function(t){ unique( char( panel[which(panel$startYear<=t),'lab'] ) ) }) }

names(actorList) = char(pds)
#################

#################
# Create adj matrices
adjList = list()
for(t in 1:length(pds)){
	actors = actorList[[t]]
	adj = matrix(0, nrow=length(actors),ncol=length(actors),dimnames=list(actors,actors))
	dataT = cleanData[cleanData$time == pds[t],actorIDs]
	for(ii in 1:nrow(dataT)){
		senders = dataT[ii,grep('sender', actorIDs)] %>% .[!is.na(.)]
		targets = dataT[ii,grep('Target', actorIDs)] %>% .[!is.na(.)]
		adj[senders,targets] = adj[senders,targets] + 1 } # count
		# adj[senders,targets] = 1 } # binomial
	adjList[[t]] = adj }
names(adjList) = char( pds )
#################

#################
# Add in government info as covariates
govActors = c('federal','state','municipal')
govIntRow = lapply(adjList, function(adj){ adj[,govActors] })
govIntCol = lapply(adjList, function(adj){ t( adj[govActors,] ) })
#################

#################
# Add in some other network measures?
betweenCalc = lapply(adjList, function(adj){
	adj[which(!rownames(adj) %in% govActors),which(!rownames(adj) %in% govActors)]
	# g = igraph::graph.adjacency(adj, mode='directed')
	# print(plot(g))
	})

library(RSiena)

#################

#################
# Add in protest data
protestData = read.csv(paste0(pathData, 'baseProtest.csv'))

actors[4]
actSub = cleanData[apply(cleanData[,actorIDs], 1, function(x) actors[4] %in% x),]
#################

#################
# Create array
actors = rownames(adjList[[1]])
adjList = lapply(adjList, function(x)x[actors,actors])
adjArr = array(unlist(adjList), 
	dim=c(length(actors), length(actors), length(adjList)), 
	dimnames=list(actors,actors,names(adjList)))
#################

#################
# Create covariates
source(paste0(fPth, 'mltrHelpers.R'))
arrCovar = createRelCovar(arr=adjArr, var='conflict', incMain=TRUE, incRecip=TRUE, incTrans=TRUE)

# exog predictors
Z = arrCovar[,,,-dim(adjArr)[3]] # lag
# DV
Y = adjArr[,,-1] # lag
# Necessary for AB calc
X = Z[,,'conflict',]
# Rand vectors for infl
W = array(dim=c(dim(Y)[1:2], 3), dimnames=list(actors,actors,c('int', 'rand', 'rand2')) )
W[,,1] = array(1, dim(Y)[1:2])
set.seed(43543) ; W[,,2] = array(rnorm(length(Y[,,1])), dim(Y)[1:2])
set.seed(98798) ; W[,,3] = array(rnorm(length(Y[,,1])), dim(Y)[1:2])

save(Y, X, Z, W, file=paste0(pathData, 'barData.rda'))
#################