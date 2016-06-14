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
cleanData = cleanData[cleanData$year>2005 & cleanData$year<2013,]
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
# Add in dto control data
dtoCntrl = read.csv(paste0(pathData, 'dtoControl.csv'), stringsAsFactors=FALSE)

# Remove extraneous columns
dtoCntrl = dtoCntrl[,-c(1, ncol(dtoCntrl)-1, ncol(dtoCntrl))]

# Clean muni data
replValCol = function(col, old, new){ col[which(col == old)] = new; return(col) }
dtoCntrl[,3:ncol(dtoCntrl)] = apply(dtoCntrl[,3:ncol(dtoCntrl)], 2, function(colSlice){
	colSlice = trim(colSlice)
	colSlice = replValCol(colSlice, '', 'N/A')
	colSlice = replValCol(colSlice, 'Aquascalientes', 'Aguascalientes')
	colSlice = replValCol(colSlice, 'Quintanan Roo', 'Quintana Roo')
	colSlice = replValCol(colSlice, 'San Luis Potisi', 'San Luis Potosi')
	colSlice = replValCol(colSlice, 'Tamauilpas', 'Tamaulipas')
	colSlice = replValCol(colSlice, 'Mexico', 'Estado de Mexico')
	return(colSlice)
})

# load protest data
protestData = read.csv(paste0(pathData, 'baseProtest.csv'))

# merge in protest cnts to actor level
dtoCntrl = dtoCntrl[dtoCntrl$year>2005 & dtoCntrl$year<2013,]
dtoCntrl$protest = 0
for(ii in 1:nrow(dtoCntrl)){
	slice = dtoCntrl[ii,]
	states = slice[3:(length(slice)-1)] %>% .[which(!. == 'N/A')] %>% unlist()
	if(is.null(states)){ val = 0 } else {
		val = protestData$protest[which(protestData$year == slice$year & protestData$state %in% states)] %>% sum(.)
	}
	dtoCntrl$protest[ii] = val
}
#################

#################
# Create array for DV
actors = rownames(adjList[[1]])
adjList = lapply(adjList, function(x)x[actors,actors])
adjArr = array(unlist(adjList), 
	dim=c(length(actors), length(actors), length(adjList)), 
	dimnames=list(actors,actors,names(adjList)))

# Create array for monadic protest data
protArr = array(0, dim=c(length(actors), length(actors), 2, length(adjList)), 
	dimnames=list(actors,actors,c('protestRow', 'protestCol'),names(adjList)))
for(t in 1:dim(protArr)[4]){
	yr=strsplit(dimnames(protArr)[[4]][t], '-') %>% lapply(., function(x){ x[1] }) %>% unlist() %>% num()
	dtoProt = dtoCntrl[dtoCntrl$year==yr,c('actor','year','protest')] %>% .[match(.[,'actor'], actors),] %>% na.omit(.)
	protArr[,,1,t] = matrix(dtoProt$protest, nrow=length(actors), ncol=length(actors), byrow=FALSE)
	protArr[,,2,t] = matrix(dtoProt$protest, nrow=length(actors), ncol=length(actors), byrow=FALSE)
}
#################

#################
# Create covariates
source(paste0(fPth, 'mltrHelpers.R'))
arrCovar = createRelCovar(arr=adjArr, var='conflict', incMain=TRUE, incRecip=TRUE, incTrans=TRUE)

# exog predictors
Z = array(0, 
	dim=append(dim(arrCovar)[c(1,2,4)], dim(arrCovar)[3] + dim(protArr)[3], after=2),
	dimnames=list(actors,actors,c(dimnames(arrCovar)[[3]], dimnames(protArr)[[3]]),names(adjList)) )
Z[,,1:3,] = arrCovar
Z[,,4:5,] = protArr
Z = Z[,,,-dim(adjArr)[3]] # lag
# remove protest col
Z = Z[,,-5,]

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