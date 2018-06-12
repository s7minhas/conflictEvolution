################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R') }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R') }
loadPkg('devtools') ; devtools::install_github('s7minhas/amen') ; library(amen)
################

################
useActors = c("Boko Haram", "Christian Militia (Nigeria)",
	"Fulani Ethnic Militia (Nigeria)",
    "Hausa Ethnic Militia (Nigeria)",
    "Igbo Ethnic Militia (Nigeria)",
    "Ijaw Ethnic Militia (Nigeria)",
    "NDV: Niger Delta Vigilante",
    "NDPVF: Niger Delta People's Volunteer Force",
    "Ilajes Ethnic Militia (Nigeria)",
    "Itsekiri Ethnic Militia (Nigeria)",
    "Itsekiri Ethnic Militia (Nigeria)",
    "Kuteb Ethnic Militia (Nigeria)",
    "Military Forces of Nigeria",
    "Muslim Militia (Nigeria)",
    "Ogoni Ethnic Militia (Nigeria)",
    "Police Forces of Nigeria",
    "Tarok Ethnic Militia (Nigeria)",
    "Tiv Ethnic Militia (Nigeria)",
    "Urhobo Ethnic Militia (Nigeria)",
    "Yoruba Ethnic Militia (Nigeria)")

useActors = c(useActors, "MEND: Movement for the Emancipation of the Niger Delta")
################

################
# load data
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object 
load(paste0(pathData, 'exoVars.rda')) # load xNodeL, xDyadL

# only acled & ucdp actors
yList = lapply(yList, function(y){
	inAcledUcdp = intersect(rownames(y), useActors)
	return( y[inAcledUcdp,inAcledUcdp] ) })

# focus on post 2000 data [few actors beforehand]
yrs = char(2000:2016)
yList = yList[yrs] ; xDyadL = xDyadL[yrs] ; xNodeL = xNodeL[yrs]

# manage actor diffs
xAll = lapply(1:length(yrs), function(t){
	actors = rownames(yList[[t]])
	xDyad = xDyadL[[t]][actors,actors,]
	xNode = xNodeL[[t]][actors,]
	return(list(xDyad=xDyad, xNode=xNode)) })

#
xDyadL = lapply(xAll, function(x){x$xDyad}) ; names(xDyadL) = yrs
xNodeL = lapply(xAll, function(x){x$xNode}) ; names(xNodeL) = yrs
###############

###############
# make yList symmetric
yList = lapply(yList, function(y){
	diag(y) = NA ; ySymm = y + t(y) ; ySymm[ySymm>1] = 1
	return(ySymm) })
###############

################
# set up model specs
subListArray = function(lA, vars, dims=2){
	if(dims==2){ return( lapply(lA, function(x){ x[,vars, drop=FALSE] }) ) }
	if(dims==3){ return( lapply(lA, function(x){ x[,,vars,drop=FALSE] }) ) } }
designArrays = list(
	NULL=list(senCovar=NULL, recCovar=NULL, dyadCovar=NULL), 
	base=list(
		senCovar=subListArray(xNodeL, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2),
		recCovar=subListArray(xNodeL, c('riotsProtestsAgainst', 'vioCivEvents', 'groupSpread'), 2),
		dyadCovar=subListArray(xDyadL, c('govActor', 'postBoko', 'elecYear', 'ngbrConfCount'), 3)
		),
	rioProSep=list(
		senCovar=subListArray(xNodeL, c('riotsAgainst', 'protestsAgainst','vioCivEvents', 'groupSpread'), 2),
		recCovar=subListArray(xNodeL, c('riotsAgainst', 'protestsAgainst','vioCivEvents', 'groupSpread'), 2),
		dyadCovar=subListArray(xDyadL, c('govActor', 'postBoko', 'elecYear', 'ngbrConfCount'), 3)
		)
	)

# parallelize model run
loadPkg(c('parallel','foreach'))
cores=length(designArrays) ; cl=makeCluster(cores) ; registerDoParallel(cl)
ameFits = foreach(i = 1:length(designArrays), .packages=c('amen')) %dopar% {
	fit = ame_repL(
		Y=yList, Xdyad=designArrays[[i]]$dyadCovar,
		Xrow=designArrays[[i]]$senCovar,
		symmetric=TRUE, nvar=TRUE, R=2, 
		model='bin', intercept=TRUE, seed=6886,
		burn=10000, nscan=50000, odens=25,
		plot=FALSE, gof=TRUE, periodicSave=FALSE )
	return(fit)
} ; stopCluster(cl)
names(ameFits) = names(designArrays)
################

################
# save
save(
	ameFits, designArrays,
	file=paste0(pathResults, 'ameResults_ucdp_symmetric.rda')
	)
################