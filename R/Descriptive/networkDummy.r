library(igraph)

#build a random network
num_nodes <- 10
my_sociomatrix <- matrix(round(runif(num_nodes*num_nodes)), 
                  nrow = num_nodes, 
                  ncol = num_nodes)

diag(my_sociomatrix) <- 0

my_sociomatrix1<-my_sociomatrix
g1<-graph_from_adjacency_matrix(my_sociomatrix1, mode = c("directed"),
	 weighted = TRUE, diag = FALSE,
  add.colnames = NULL, add.rownames = NA)

#plot it 
plot(g1, edge.arrow.size=.5, 
	vertex.color="black", vertex.size=15, 
	vertex.frame.color="gray", vertex.label=NA, 
	edge.curved=0.1)

#change it 
my_sociomatrix[2,3]<-0
my_sociomatrix[4,1]<-0
my_sociomatrix[7,2]<-0
my_sociomatrix[3,7]<-4
my_sociomatrix[10,1]<-5

g2<-graph_from_adjacency_matrix(my_sociomatrix, mode = c("directed"),
	 weighted = TRUE, diag = FALSE,
  add.colnames = NULL, add.rownames = NA)

plot(g2, edge.arrow.size=.5, 
	vertex.color="black", vertex.size=15, 
	vertex.frame.color="gray", vertex.label=NA, 
	edge.curved=0.1) 

#############----------------------------
#maybe making a simple matrix is better 
#############----------------------------
set.seed(12345)
A = matrix(c(0, 0, 0, 1, 2, 
			 4, 5, 0, 0, 1),
	nrow=5,            
	ncol=5,          
 	byrow = TRUE)

a<-graph_from_adjacency_matrix(A, mode = c("directed"),
	 weighted = TRUE, diag = FALSE,
  add.colnames = NULL, add.rownames = NA)

V(a)$color[c(1, 2, 4, 5)] <- "grey"
V(a)$color[3] <- "black"

plot(a, edge.arrow.size=.5, 
	 vertex.size=15, 
	vertex.frame.color="gray", vertex.label=NA, 
	edge.curved=0.1, layout=layout.fruchterman.reingold,
	edge.width=E(a)$weight/2, vertex.color=V(g)$color) 

##mat 2
B = matrix(c(0, 0, 0, 1, 2, 
			 4, 0, 0, 0, 1,
			 0, 0, 0, 1, 2,
			 0, 0, 3, 4, 2,
			 0, 0, 2, 4, 3,
			 0, 0, 0, 1, 2),
	nrow=6,            
	ncol=6,          
 	byrow = TRUE)

b<-graph_from_adjacency_matrix(B, mode = c("directed"),
	 weighted = TRUE, diag = FALSE,
  add.colnames = NULL, add.rownames = NA)

V(b)$color[c(1,4,5,6)] <- "grey"
V(b)$color[c(3,2)] <- "black"


plot(b, edge.arrow.size=.5, 
	vertex.size=15, 
	vertex.frame.color="gray", vertex.label=NA, 
	edge.curved=0.1, layout=layout.fruchterman.reingold,
	edge.width=E(a)$weight/2)

##mat 3
C = matrix(c(0, 0, 0, 1, 2, 
			 4, 5, 0, 0, 1,
			 0, 0, 0, 1, 2,
			 0, 0, 3, 4, 2,
			 0, 0, 3, 4, 3,
			 0, 0, 0, 1, 2,
			 1, 1, 4, 2, 1,
			 4, 5, 6, 1, 3),
	nrow=8,            
	ncol=8,          
 	byrow = TRUE)

c<-graph_from_adjacency_matrix(C, mode = c("directed"),
	 weighted = TRUE, diag = FALSE,
  add.colnames = NULL, add.rownames = NA)

V(b)$color[c(1,4,5,6, 7)] <- "grey"
V(b)$color[c(3,2, 8)] <- "black"

plot(c, edge.arrow.size=.5, 
	vertex.size=15, 
	vertex.frame.color="gray", vertex.label=NA, 
	edge.curved=0.1, layout=layout.fruchterman.reingold,
	edge.width=E(a)$weight/2)
