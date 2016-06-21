################################################################
## analysis paper 3 - plotting the raw network data by year
################################################################

#data for DV
load(paste0(pathData, "barDataYearly.rda"))

#functions
delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0) - 1
  delete.vertices(graph, isolates)
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#adjustments to the matrices for plotting
dimnames(Y)[[1]] = dimnames(Y)[[2]] = sapply(dimnames(Y)[[1]], simpleCap)
names<-rownames(Z[1,,,])
for(slice in 1:dim(Y)[3]){
  diag(Y[,,slice]) = 0
}
ind<-c("gov", "gov", "gov", "c", "c", "c", "c", "c", "c")
color<-cbind(names,ind)

# plot style from igraph- easier to interpret overtime
# 2007
graph2007<-graph.adjacency(as.matrix(Y[,,1]), mode="directed", weighted=TRUE)
V(graph2007)$size=degree(graph2007)

graph2007$type=color[,2]
V(graph2007)$color=graph2007$type
V(graph2007)$color=gsub("c", "aquamarine3", V(graph2007)$color)
V(graph2007)$color=gsub("gov", "burlywood", V(graph2007)$color)
V(graph2007)$size=degree(graph2007)+1
V(graph2007)$label.cex=.6

plot(graph2007, layout=layout.circle, vertex.size=11, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.7, edge.width=E(graph2007)$weight,
     vertex.label.family="Helvetica", edge.curved=seq(.1, .1), edge.arrow.size=.5)
text(1.2,1.2, "2007", font=2)

# 2008
graph2008<-graph.adjacency(as.matrix(Y[,,2]), mode="directed", weighted=TRUE)
V(graph2008)$size=degree(graph2008)

graph2008$type=color[,2]
V(graph2008)$color=graph2008$type
V(graph2008)$color=gsub("c", "aquamarine3", V(graph2008)$color)
V(graph2008)$color=gsub("gov", "burlywood", V(graph2008)$color)
V(graph2008)$size=degree(graph2008)+1
V(graph2008)$label.cex=.6

plot(graph2008, layout=layout.circle, vertex.size=11, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.7, edge.width=E(graph2008)$weight,
     vertex.label.family="Helvetica", edge.curved=seq(.1, .1), edge.arrow.size=.5)
text(1.2,1.2, "2008", font=2)

# 2009
graph2009<-graph.adjacency(as.matrix(Y[,,3]), mode="directed", weighted=TRUE)
V(graph2009)$size=degree(graph2009)

graph2009$type=color[,2]
V(graph2009)$color=graph2009$type
V(graph2009)$color=gsub("c", "aquamarine3", V(graph2009)$color)
V(graph2009)$color=gsub("gov", "burlywood", V(graph2009)$color)
V(graph2009)$size=degree(graph2009)+1
V(graph2009)$label.cex=.6

plot(graph2009, layout=layout.circle, vertex.size=11, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.7, edge.width=E(graph2009)$weight,
     vertex.label.family="Helvetica", edge.curved=seq(.1, .1), edge.arrow.size=.5)
text(1.2,1.2, "2009", font=2)

# 2010
graph2010<-graph.adjacency(as.matrix(Y[,,4]), mode="directed", weighted=TRUE)
V(graph2010)$size=degree(graph2010)

graph2010$type=color[,2]
V(graph2010)$color=graph2010$type
V(graph2010)$color=gsub("c", "aquamarine3", V(graph2010)$color)
V(graph2010)$color=gsub("gov", "burlywood", V(graph2010)$color)
V(graph2010)$size=degree(graph2010)+1
V(graph2010)$label.cex=.6

plot(graph2010, layout=layout.circle, vertex.size=11, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.7, edge.width=E(graph2010)$weight,
     vertex.label.family="Helvetica", edge.curved=seq(.1, .1), edge.arrow.size=.5)
text(1.2,1.2, "2010", font=2)

# 2011
graph2011<-graph.adjacency(as.matrix(Y[,,5]), mode="directed", weighted=TRUE)
V(graph2011)$size=degree(graph2011)

graph2011$type=color[,2]
V(graph2011)$color=graph2011$type
V(graph2011)$color=gsub("c", "aquamarine3", V(graph2011)$color)
V(graph2011)$color=gsub("gov", "burlywood", V(graph2011)$color)
V(graph2011)$size=degree(graph2011)+1
V(graph2011)$label.cex=.6

plot(graph2011, layout=layout.circle, vertex.size=11, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.7, edge.width=E(graph2011)$weight,
     vertex.label.family="Helvetica", edge.curved=seq(.1, .1), edge.arrow.size=.5)
text(1.2,1.2, "2011", font=2)

# 2012
graph2012<-graph.adjacency(as.matrix(Y[,,6]), mode="directed", weighted=TRUE)
V(graph2012)$size=degree(graph2012)

graph2012$type=color[,2]
V(graph2012)$color=graph2012$type
V(graph2012)$color=gsub("c", "aquamarine3", V(graph2012)$color)
V(graph2012)$color=gsub("gov", "burlywood", V(graph2012)$color)
V(graph2012)$size=degree(graph2012)+1
V(graph2012)$label.cex=.6

plot(graph2012, layout=layout.circle, vertex.size=11, vertex.label.color= "black", 
     edge.curved=seq(.1, .1), vertex.label.dist=.7, edge.width=E(graph2012)$weight,
     vertex.label.family="Helvetica", edge.curved=seq(.1, .1), edge.arrow.size=.5)
text(1.2,1.2, "2012", font=2)
