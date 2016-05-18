# setup
source('~/Research/conflictEvolution/R/setup.R')

#data for DV
load(paste0(pathData, "matListCrime.rda")) #change to matListCrime0516.rda
load(paste0(pathData, "matListCrimeBin.rda"))
load(paste0(pathData, "matListCrimeOrd.rda"))

#data for Nodal covariates
load(paste0(pathData, "allNodal.rda"))
load(paste0(pathData, "allNodalLag.rda"))

#one slice matrices
dv<-as.array(matListCrime[1:9])
dvOrd<-as.array(matListCrimeOrd[1:9])
dvBin<-as.array(matListCrimeBin[1:9])

#homogenous actors across time & space/temporal model 
#make Y
years<-seq(2005, 2013, 1)
first<-colnames(matListCrimeOrd[[1]])
second<-rownames(matListCrimeOrd[[1]])
third<-as.character(years)
allNames<-list(first,second,third)
dvMats<-array(unlist(matListCrimeBin), dim=c(nrow(matListCrimeBin[[1]]), ncol(matListCrimeBin[[1]]), 
          length(matListCrimeBin)), dimnames=allNames)
dvMatsOrd<-array(unlist(matListCrimeOrd), dim=c(nrow(matListCrimeOrd[[1]]), ncol(matListCrimeOrd[[1]]), 
          length(matListCrimeOrd)), dimnames=allNames)
# Transform Y so that events only occur between dto-gov and dto-dto
govActors = dimnames(dvMats)[[1]][c(1:3,6:9)]
dtoActors = dimnames(dvMats)[[1]][c(4:5,10:17)]

# DV mods
# pathResults ='~/Dropbox/Research/conflictEvolution/results/all_interactions/'
dvMats[govActors,govActors,] = 0
dvMatsOrd[govActors,govActors,] = 0

#load nodal covariates 
Xnode = allNodal[,,as.character(2007:2013)]
yBin = dvMats[,,as.character(2007:2013)]
yOrd = dvMatsOrd[,,as.character(2007:2013)]

# Transform design matrix
varNames = dimnames(Xnode)[[2]][which(!dimnames(Xnode)[[2]] %in% 'govI')]
varNames = c(varNames, 'dto' )
Xnodev2 = array(NA, 
    dim=c(dim(Xnode)[1], length(varNames), dim(Xnode)[3]), 
    dimnames=list(dimnames(Xnode)[[1]], varNames, dimnames(Xnode)[[3]]) )
for(ii in 1:dim(Xnodev2)[3]){
    x = Xnode[,,ii]
    dto = ifelse(x[,'govI']==1, 0, 1)
    x = cbind(x, dto)
    x = x[,which(!colnames(x) %in% 'govI')]
    Xnodev2[,,ii] = x
}

# Set up model specs
modList = list(
    net = list(yBin=yBin, yOrd=yOrd, Xnode=Xnodev2[,c('centrality', 'betweeness'),], Xdyad=NULL, name='net'),
    protest = list(yBin=yBin, yOrd=yOrd, Xnode=Xnodev2[,c('protestLagCount'),,drop=FALSE], Xdyad=NULL, name='protest'),            
    protest_net = list(yBin=yBin, yOrd=yOrd, Xnode=Xnodev2[,c('centrality', 'betweeness', 'protestLagCount'),,drop=FALSE], Xdyad=NULL, name='protest_net'),
    protest_net_dto = list(yBin=yBin, yOrd=yOrd, Xnode=Xnodev2[,c('centrality', 'betweeness', 'protestLagCount','dto'),,drop=FALSE], Xdyad=NULL, name='protest_net_dto')
    )
save(modList, file=paste0(pathResults, 'modList.rda'))

# # Run binary mods
iter=10000 ; toBurn=1001 ; odes=10
print('Running binary amen models...')
cl = makeCluster(4)
registerDoParallel(cl)
foreach(mod = names(modList), .packages=c('amen')) %dopar% {
    fit = ame_rep(Y=modList[[mod]]$'yBin', Xdyad=modList[[mod]]$'Xdyad', modList[[mod]]$'Xnode', model='bin',
        symmetric=TRUE, burn=toBurn, nscan=iter, odens=odes, plot=FALSE, print = FALSE )
    save(fit, file=paste0(pathResults, modList[[mod]]$'name', '_bin.rda') ) }
stopCluster(cl)

# Run ordinal mods
print('Running ordinal amen models...')
cl = makeCluster(4)
registerDoParallel(cl)
foreach(mod = names(modList), .packages=c('amen')) %dopar% {
    fit = ame_rep(Y=modList[[mod]]$'yOrd', Xdyad=modList[[mod]]$'Xdyad', modList[[mod]]$'Xnode', model='ord',
        symmetric=TRUE, burn=toBurn, nscan=iter, odens=odes, plot=FALSE, print = FALSE )
    save(fit, file=paste0(pathResults, modList[[mod]]$'name', '_ord.rda')) }
stopCluster(cl)