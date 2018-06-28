# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
  source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
  source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
  source('~/Documents/conflictEvolution/R/setup.R')  }

loadPkg(c('car', 'abind', 'spatstat'))
load(paste0(pathData, "nData.rda"))
load(paste0(pathData,"exovars.rda"))
xDyadVars = dimnames(xDyadL[[1]])[[3]]
xNodeVars = dimnames(xNodeL[[1]])[[2]]


latlong = function(group, yr, type = "LATITUDE"){
  return(c(nData[which(nData$a1 == group & nData$YEAR == yr), type], 
    nData[which(nData$a2 == group & nData$YEAR == yr), type]))
}

# for(i in 1:length(xNodeL)){
#   yr = names(xNodeL)[i]
  
#   png(paste0(pathGraphics, "GeoPlots/GovEllipse", yr, ".png"), width = 640, height = 640)
#   groups = rownames(xNodeL[[i]])
#   locData = c() 
#   shortData = c()
#   for(j in 1:length(groups)){
#     lat = latlong(groups[j], yr, "LATITUDE")
#     lon = latlong(groups[j], yr, "LONGITUDE")
#     labels = rep(groups[j], length(lat))
#     if(length(unique(lat)) > 2 | length(unique(lon)) > 2){
#     locData = rbind(locData, cbind(lat, lon, labels))}
#     if(length(unique(lat)) <= 2 & length(unique(lon)) <= 2){
#     shortData = rbind(shortData, cbind(lat, lon, labels))
#     }
#   }
#   locData = data.frame(locData)
#   locData$lat = as.numeric(as.character(locData$lat))
#   locData$lon = as.numeric(as.character(locData$lon))
#   shortData = data.frame(shortData)
#   col = rainbow(length(levels(locData$labels)) + length(levels(shortData$labels)))
#   dataEllipse(locData$lon, locData$lat,  xlim = c(2, 14), ylim = c(4.5,14), 
#     groups = locData$labels, col = col[1:length(levels(locData$labels))])
#   shortData$lat = as.numeric(as.character(shortData$lat))
#   shortData$lon = as.numeric(as.character(shortData$lon))
# if(dim(shortData)[1] != 0){
#   latShort = summaryBy(lat~labels, data = shortData, FUN = mean)  
#   lonShort = summaryBy(lon~labels, data = shortData, FUN = mean)  
#   text(latShort$lat.mean, lonShort$lon.mean, levels(shortData$labels), 
#     col= col[(length(levels(locData$labels)) + 1):length(col)])}
#   dev.off()
# }

# for(i in 1:length(xNodeL)){
#   yr = names(xNodeL)[i]
  
#   png(paste0(pathGraphics, "GeoPlots/NoGovEllipse", yr, ".png"), width = 640, height = 640)
#   groups = rownames(xNodeL[[i]])
#   locData = c() 
#   shortData = c()
#   for(j in 1:length(groups)){
#     if(!groups[j] %in% c("Military Forces of Nigeria", "Police Forces of Nigeria")){
#     lat = latlong(groups[j], yr, "LATITUDE")
#     lon = latlong(groups[j], yr, "LONGITUDE")
#     labels = rep(groups[j], length(lat))
#     if(length(unique(lat)) > 2 | length(unique(lon)) > 2){
#       locData = rbind(locData, cbind(lat, lon, labels))}
#     if(length(unique(lat)) <= 2 & length(unique(lon)) <= 2){
#       shortData = rbind(shortData, cbind(lat, lon, labels))
#     }}
#   }
#   locData = data.frame(locData)
#   locData$lat = as.numeric(as.character(locData$lat))
#   locData$lon = as.numeric(as.character(locData$lon))
#   shortData = data.frame(shortData)
#   col = rainbow(length(levels(locData$labels)) + length(levels(shortData$labels)))
#   dataEllipse(locData$lon, locData$lat, xlim = c(2, 14), ylim = c(4.5,14), 
#     groups = locData$labels, col = col[1:length(levels(locData$labels))])
#   shortData$lat = as.numeric(as.character(shortData$lat))
#   shortData$lon = as.numeric(as.character(shortData$lon))
#   if(dim(shortData)[1] != 0){
#     latShort = summaryBy(lat~labels, data = shortData, FUN = mean)  
#     lonShort = summaryBy(lon~labels, data = shortData, FUN = mean)  
#     text(latShort$lat.mean, lonShort$lon.mean, levels(shortData$labels), 
#       col= col[(length(levels(locData$labels)) + 1):length(col)])}
#   dev.off()
# }

# add covar to measure dist between group activities
for(i in 1:length(xDyadL)){
  yr = names(xDyadL)[i]
  groups = rownames(xDyadL[[i]])
  coords = c()
  for(j in 1:length(groups)){
      lat = c()
      lon = c()
      for(k in 1:3){
      lat = c(lat,latlong(groups[j], as.numeric(yr) - i , "LATITUDE"))
      lon = c(lon, latlong(groups[j], as.numeric(yr) - i, "LONGITUDE"))}
      coords = rbind(coords, c(median(lat), median(lon)))
  }
  dists = as.matrix(dist(coords, upper = T, diag = T))
  xDyadL[[i]] = abind(xDyadL[[i]], dists, along = 3)
  dimnames(xDyadL[[i]]) = list(groups, groups, c(xDyadVars, 'medianDist'))
}

# add actor level dispersion
for(i in 1:length(xNodeL)){
  yr = names(xDyadL)[i]
  groups = rownames(xDyadL[[i]])
  groupspread = numeric(length(groups))
  coords = c()
  for(j in 1:length(groups)){
    lat = c()
    lon = c()
    for(k in 1:3){
      lat = c(lat,latlong(groups[j], as.numeric(yr) - i , "LATITUDE"))
      lon = c(lon, latlong(groups[j], as.numeric(yr) - i, "LONGITUDE"))}
  mean.c = c(mean(lat), mean(lon))
  groupspread[j] = mean(as.matrix(dist(rbind(mean.c, cbind(lat, lon)), upper = T, diag = T))[,1])}
  xNodeL[[i]] = cbind(xNodeL[[i]], groupspread)
  dimnames(xNodeL[[i]]) = list(groups, c(xNodeVars, 'groupSpread'))
}

save(xDyadL, xNodeL, file = paste0(pathData,"exoVars.rda"))