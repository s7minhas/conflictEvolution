################################################################
## analysis paper 3 - plotting the raw network data by year
## updated data code (fall 2015)
## Cassy Dorff
################################################################

#data for DV
load(paste0(pathData,"matListCrime.rda"))
load(paste0(pathData,"matListCrimeBin.rda"))
load(paste0(pathData,"matListCrimeOrd.rda"))
load(paste0(pathData,"allNodal.rda"))

#presets
delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0) - 1
  delete.vertices(graph, isolates)
}

color<-c("cadetblue4", "cadetblue4", "cadetblue4", "blue", "blue", "cadetblue4", 
         "cadetblue4", "cadetblue4", "cadetblue4", 
         "blue", "blue", "blue","blue","blue","blue","blue","blue")

govIndColor<-data.frame(cbind(allNodal[,2,1], color))

#one plot style from amen package
X<-xnet(dvOrd[[1]])
netplot(dvOrd[[1]],X, ncol=as.character(govIndColor$color), plot.iso=FALSE, pch=19)

X<-xnet(dvOrd[[2]])
netplot(dvOrd[[2]],X, ncol=as.character(govIndColor$color), plot.iso=FALSE, pch=19)

X<-xnet(dvOrd[[3]])
netplot(dvOrd[[3]],X, ncol=as.character(govIndColor$color), plot.iso=FALSE, pch=19)

X<-xnet(dvOrd[[4]])
netplot(dvOrd[[4]],X, ncol=as.character(govIndColor$color), plot.iso=FALSE, pch=19)

# plot style from igraph- easier to interpret overtime
# 2005
grFive<-graph.adjacency(as.matrix(dvOrd[[1]]), mode="undirected", weighted=TRUE)
V(grFive)$size=degree(grFive) #size of node be degree

grFive$type=govIndColor$V1
V(grFive)$color=grFive$type
V(grFive)$color=gsub("1", "aquamarine3", V(grFive)$color)
V(grFive)$color=gsub("2", "burlywood", V(grFive)$color)
V(grFive)$size=degree(grFive)+1

plot(grFive, layout=layout.circle, vertex.size=11, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.7, edge.width=E(grFive)$weight*2,
     vertex.label.family="Helvetica", edge.curved=seq(.1, .1), vertex.label=NA)
text(1,1, "2005", font=2)

# 2006
grSix<-graph.adjacency(as.matrix(dvOrd[[2]]), mode="undirected", weighted=TRUE)
V(grSix)$size=degree(grSix) #size of node be degree

grSix$type=govIndColor$V1
V(grSix)$color=grSix$type
V(grSix)$color=gsub("1", "aquamarine3", V(grSix)$color)
V(grSix)$color=gsub("2", "burlywood", V(grSix)$color)
V(grSix)$size=degree(grSix)+1

plot(grSix, layout=layout.circle, vertex.size=11, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.7, edge.width=E(grSix)$weight*2,
     vertex.label.family="Helvetica", edge.curved=seq(.1, .1), vertex.label=NA)
text(1,1, "2006", font=2)

# 2007
grSeven<-graph.adjacency(as.matrix(dvOrd[[3]]), mode="undirected", weighted=TRUE)
V(grSeven)$size=degree(grSeven) #size of node be degree

grSeven$type=govIndColor$V1
V(grSeven)$color=grSeven$type
V(grSeven)$color=gsub("1", "aquamarine3", V(grSeven)$color)
V(grSeven)$color=gsub("2", "burlywood", V(grSeven)$color)
V(grSeven)$size=degree(grSeven)+1

plot(grSeven, layout=layout.circle, vertex.size=11, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.7, edge.width=E(grSeven)$weight*2,
     vertex.label.family="Helvetica", edge.curved=seq(.1, .1), vertex.label=NA)
text(1,1, "2007", font=2)

# 2008
grEight<-graph.adjacency(as.matrix(dvOrd[[4]]), mode="undirected", weighted=TRUE)
V(grEight)$size=degree(grEight)

grEight$type=govIndColor$V1
V(grEight)$color=grEight$type
V(grEight)$color=gsub("1", "aquamarine3", V(grEight)$color)
V(grEight)$color=gsub("2", "burlywood", V(grEight)$color)
V(grEight)$size=degree(grEight)+1

plot(grEight, layout=layout.circle, vertex.size=11, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.7, edge.width=E(grEight)$weight*2,
     vertex.label.family="Helvetica", edge.curved=seq(.1, .1))
text(1,1, "2008", font=2)

# 2009
grNine<-graph.adjacency(as.matrix(dvOrd[[5]]), mode="undirected", weighted=TRUE)
V(grNine)$size=degree(grNine)

grNine$type=govIndColor$V1
V(grNine)$color=grNine$type
V(grNine)$color=gsub("1", "aquamarine3", V(grNine)$color)
V(grNine)$color=gsub("2", "burlywood", V(grNine)$color)
V(grNine)$size=degree(grNine)+1

plot(grNine, layout=layout.circle, vertex.size=11, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.7, edge.width=E(grNine)$weight*2,
     vertex.label.family="Helvetica", edge.curved=seq(.1, .1), vertex.label=NA)
text(1,1, "2009", font=2)

# 2010
grTen<-graph.adjacency(as.matrix(dvOrd[[6]]), mode="undirected", weighted=TRUE)
V(grTen)$size=degree(grTen)

grTen$type=govIndColor$V1
V(grTen)$color=grTen$type
V(grTen)$color=gsub("1", "aquamarine3", V(grTen)$color)
V(grTen)$color=gsub("2", "burlywood", V(grTen)$color)
V(grTen)$size=degree(grTen)+1

plot(grTen, layout=layout.circle, vertex.size=11, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.7, edge.width=E(grTen)$weight*2,
     vertex.label.family="Helvetica", edge.curved=seq(.1, .1), vertex.label=NA)
text(1,1, "2010", font=2)

# 2011
grEleven<-graph.adjacency(as.matrix(dvOrd[[7]]), mode="undirected", weighted=TRUE)
V(grEleven)$size=degree(grEleven)

grEleven$type=govIndColor$V1
V(grEleven)$color=grEleven$type
V(grEleven)$color=gsub("1", "aquamarine3", V(grEleven)$color)
V(grEleven)$color=gsub("2", "burlywood", V(grEleven)$color)
V(grEleven)$size=degree(grEleven)+1

plot(grEleven, layout=layout.circle, vertex.size=11, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.7, edge.width=E(grEleven)$weight*2,
     vertex.label.family="Helvetica", edge.curved=seq(.1, .1), vertex.label=NA)
text(1,1, "2011", font=2)

# 2012
grTwelve<-graph.adjacency(as.matrix(dvOrd[[8]]), mode="undirected", weighted=TRUE)
V(grTwelve)$size=degree(grTwelve)

grTwelve$type=govIndColor$V1
V(grTwelve)$color=grTwelve$type
V(grTwelve)$color=gsub("1", "aquamarine3", V(grTwelve)$color)
V(grTwelve)$color=gsub("2", "burlywood", V(grTwelve)$color)
V(grTwelve)$size=degree(grTwelve)+1

plot(grTwelve, layout=layout.circle, vertex.size=11, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.7, edge.width=E(grTwelve)$weight*2,
     vertex.label.family="Helvetica", edge.curved=seq(.1, .1), vertex.label=NA)
text(1,1, "2012", font=2)

# 2012
grTwelve<-graph.adjacency(as.matrix(dvOrd[[8]]), mode="undirected", weighted=TRUE)
V(grTwelve)$size=degree(grTwelve)

grTwelve$type=govIndColor$V1
V(grTwelve)$color=grTwelve$type
V(grTwelve)$color=gsub("1", "aquamarine3", V(grTwelve)$color)
V(grTwelve)$color=gsub("2", "burlywood", V(grTwelve)$color)
V(grTwelve)$size=degree(grTwelve)+1

plot(grTwelve, layout=layout.circle, vertex.size=11, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.7, edge.width=E(grTwelve)$weight*2,
     vertex.label.family="Helvetica", edge.curved=seq(.1, .1))
text(1,1, "2012", font=2)

