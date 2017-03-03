# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
  source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
  source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
  source('~/Documents/conflictEvolution/R/setup.R')  }
#################


load(paste0(pathData, "nData.rda"))
load(paste0(pathData,"exovars.rda"))

latlong = function(actor,year, type = "LATITUDE" ){
  return(c(nData[which((nData$YEAR == year) & (nData$a1 == actor)),type],nData[which((nData$YEAR == year) & (nData$a2 == actor)),type]))
}


names(xNodeL)
sapply(rownames(xNodeL$'2000'), FUN = function(x) mean(latlong(x, year = 2000))  )
min(nData$YEAR)
latlong("Boko Haram", 2009, "LATITUDE")                                                                                      )
for(i in 1:length(xNodeL)){
locData[[i]] = matrix(0, nrow = dim(xNodeL[[i]])[1], ncol = 6)
rownames(locData[[i]]) = rownames(xNodeL[[i]])
colnames(locData[[i]]) = c("Lat.Center", "Lat.Q025", "Lat.Q975", "Long.Center", "Long.Q025", "Long.Q975")
locData[[i]][,1] = sapply(rownames(xNodeL[[i]]), FUN = function(x) mean(latlong(x, year = names(xNodeL)[i], "LATITUDE"))  )
locData[[i]][,2:3] = t(sapply(rownames(xNodeL[[i]]), FUN = function(x) quantile(latlong(x, year = names(xNodeL)[i], "LATITUDE"), c(0.025, 0.975))  ))

locData[[i]][,4] = sapply(rownames(xNodeL[[i]]), FUN = function(x) mean(latlong(x, year = names(xNodeL)[i], "LONGITUDE"))  )
locData[[i]][,5:6] = t(sapply(rownames(xNodeL[[i]]), FUN = function(x) quantile(latlong(x, year = names(xNodeL)[i], "LONGITUDE"), c(0.025, 0.975))  ))
locData[[i]][which(is.nan(locData[[i]]))] = NA}


save(locData, file = paste0(pathData, "locData.rda"))

