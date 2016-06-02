#################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }
#################

#################
# Run mod
load(paste0(pathData, 'barData.rda'))
source(paste0(fPth, 'tfunctions.r'))
source(paste0(fPth, "poisblr_functions.R"))

# run als approach
tab <- poisblr_alsfit_speedglm(Y,W,X,Z,trace=FALSE)$tab
gH <- mll_gH(tab,Y,W,X,Z) 
ses_tab<-sqrt( diag( solve(gH$hess) %*% gH$shess %*% solve(gH$hess) )) 

p<-dim(W)[3]
q<-dim(Z)[3]
theta<-tab[1:q]
ses_theta<-ses_tab[1:q]

alpha<-c(1,tab[(q+1):(q+p-1)] )
beta<-tab[-(1:(q+p-1))]
A<-amprod(W,t(alpha),3 )[,,1]
A<-A*sign(mean(diag(A))) ; diag(A)<-0
rownames(A)<-colnames(A)<-toupper(rownames(Y))

B<-amprod(W,t(beta),3 )[,,1]
B<-B*sign(mean(diag(B))) ; diag(B)<-0
rownames(B)<-colnames(B)<-toupper(rownames(Y))
#################

#################
# Save mod results
save(theta, ses_theta, A, B, file=paste0(pathResults, 'barResults.rda'))
#################