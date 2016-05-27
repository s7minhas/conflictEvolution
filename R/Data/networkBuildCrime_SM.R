#################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }
#################

#################
# Some parameters for network data
timeLevel = 'quarterly' # yearly, quarterly, monthly
#################

#################
# Load data
cleanData = read.csv(paste0(pathData, "mexicoVioStoriesFinal.csv")) #1051 obs
# Drop desig obs in cleanData
cleanData = cleanData[which(cleanData$drop1==0 & cleanData$directional==1 ),]
# Add var to count
cleanData$event = 1

panel = read.csv(paste0(pathData,"violentActors.csv")) #updated actor list
# Pull out actor list for adj matrices
actors = panel$Actor.Name %>% unique() %>% char()
actorList = lapply(unique(panel$Year), function(x){
	actors = char( panel$Actor.Name[panel$Year==x] )
	actors = trim( actors )
	actors = tolower( actors )
	return( actors ) })
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

# Choose a date var
if(timeLevel == 'yearly'){ cleanData$time = char(cleanData$year) }
if(timeLevel == 'quarterly'){ cleanData$time = paste(cleanData$qtr, cleanData$year, sep='_') } 
if(timeLevel == 'monthly'){ cleanData$time = char(cleanData$mYr) }
#################

#################
# Subset to relvars
actorIDs=c( "senderGroup1", "senderGroup2", "senderGroup3", "TargetGroup1", "TargetGroup2", "TargetGroup3" )
keep = c( actorIDs, "mexicanState", "time" )
cleanData = cleanData[,c(keep)]
#################

#################
# Clean up sender and target vars
ugh = c('n/a','na', '')
for(var in actorIDs){
	cleanData[,var] = char( cleanData[,var]  )
	cleanData[,var] = trim(cleanData[,var])
	cleanData[,var] = tolower(cleanData[,var])
	cleanData[,var][ which(cleanData[,var] %in% ugh)  ] = NA	
}

# Determine number of cases in which all sender or all target vars are NA
cleanData$senCnt = apply(cleanData[,grep('sender',actorIDs)], 1, function(x){ sum(!is.na(x)) } )
cleanData$tarCnt = apply(cleanData[,grep('Target',actorIDs)], 1, function(x){ sum(!is.na(x)) } )
table(cleanData$senCnt) ; table(cleanData$tarCnt)

# Drop 6 sender all NA cases and 1 target all NA cases
cleanData = cleanData[which(cleanData$senCnt != 0), ]
cleanData = cleanData[which(cleanData$tarCnt != 0), ]
#################

#################
# Count up actor groupings
senders = apply(cleanData[,grep('sender',actorIDs)], 1, function(x){ x = unlist(x) %>% .[!is.na(.)] ; names(x) = NULL ; return(x) }) %>% unlist()
targets = apply(cleanData[,grep('Target',actorIDs)], 1, function(x){ x = unlist(x) %>% .[!is.na(.)] ; names(x) = NULL ; return(x) }) %>% unlist()

table(senders) %>% .[.>1] %>% .[order(.)]
table(targets) %>% .[.>1] %>% .[order(.)]
#################

#################
# Create adj matrices
adjList = list()
pds = cleanData$time %>% unique() %>% sort()
# for(t in 1:length(pds)){
	t=5
	actors = actorList[[t]]
	adj = matrix(0, nrow=length(actors),ncol=length(actors),dimnames=list(actors,actors))
	dataT = cleanData[cleanData$time == pds[t],]
	dataT$senMatch = apply(dataT[,grep('sender',actorIDs)], 1, function(x){ ifelse(sum( unlist(x) %in% actors )>0, 1, 0)  })
	dataT$tarMatch = apply(dataT[,grep('Target',actorIDs)], 1, function(x){ ifelse(sum( unlist(x) %in% actors )>0, 1, 0)  })
	dataT$match = dataT$senMatch + dataT$tarMatch
	# Drop of if actor from actorList[[t]] not in both sender and receiver
	dataT = dataT[dataT$match>1,]
	# for(ii in 1:nrow(dataT)){
		ii=1
		senders = dataT[ii,grep('sender', actorIDs)] %>% .[!is.na(.)]
		targets = dataT[ii,grep('Target', actorIDs)] %>% .[!is.na(.)]
		sum( senders %in% actors ) + sum( targets %in% actors )
	# }
# }
#################