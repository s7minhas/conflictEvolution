#### ---- functions for fitting bilinear poisson regression models
#### ---- thoughts:
#### ---- 1. use rank 1 approx to unconstrained fit for starting value


#### ---- 



#### ---- gradient and Hessian of minus log likelihood
mll_gH<-function(tab,Y=Y,W=W,X=X,Z=Z)
{
  m<-dim(Y)[1]
  p<-dim(W)[3]
  q<-dim(Z)[3]

  theta<-tab[1:q]
  alpha<-c(1,tab[(q+1):(q+p-1)] )
  beta<-tab[-(1:(q+p-1))]

  gll<-rep(0,q+2*p)
  Hll<-Sll<-matrix(0,q+2*p,q+2*p)


  for(i in 1:m){ for(j in (1:m)[-i])
  {
    yij<-Y[i,j,]
    Zij<-t(Z[i,j,,])
    Xij<-tprod(X,list(t(W[i,,]),t(W[j,,])))

    eta<-Zij%*%theta +  c( tprod(Xij,list(matrix(alpha,1,p),matrix(beta,1,p))))
    mu<-exp(eta)

    Xb<-t( amprod(Xij, matrix(beta,1,p),2)[,1,]  )
    Xa<-t( amprod(Xij, matrix(alpha,1,p),1)[1,,]  )
    if(nrow(Xb)==1){ Xb=t(Xb)  }
    if(nrow(Xa)==1){ Xa=t(Xa)  }
    Xtab<-cbind(Zij,Xb,Xa) 
   
    eX<-sweep( Xtab, 1, yij-mu,"*") 
    gll<- gll+ apply(eX,2,sum)

    Sll<-Sll+crossprod(eX) 

    rXij<-apply( sweep(Xij,3,(yij-mu),"*")  ,c(1,2),sum)
    H<- -t(Xtab) %*% sweep(Xtab,1,mu,"*")
    H[q+1:p,q+p+1:p]<- H[q+1:p,q+p+1:p] +rXij
    H[q+p+1:p,q+1:p]<- H[q+p+1:p,q+1:p] + t(rXij)
    Hll<-Hll+H
  }}

  J<-diag(q+2*p)[-(q+1),]
  list( grad= -J%*%gll , hess= -J%*%Hll%*%t(J), shess= J%*%Sll%*%t(J) )
}



#### ---- bilinear predictor
eta_tab<-function(tab,W,X,Z) 
{
  p<-dim(W)[3] 
  q<-dim(Z)[3] 

  theta<-tab[1:q] 
  alpha<-c(1,tab[(q+1):(q+p-1)] ) 
  beta<-tab[-(1:(q+p-1))]  

  A<-amprod(W, matrix(alpha,1,p),3)[,,1]
  B<-amprod(W, matrix(beta,1,p) ,3)[,,1] 
  ZT<-amprod(Z, matrix(theta,1,q) ,3)[,,1,]
  AXB<-tprod(X,list(A,B))

  ZT+AXB 
}


#### ---- minus log likelihood
mll_poisblr<-function(tab,Y,W,X,Z)
{
  ETA<-eta_tab(tab,W,X,Z)  
 -sum(Y*ETA - exp(ETA),na.rm=TRUE ) 
}


#### ---- fit via optim
poisblr_optfit<-function(Y,W,X,Z,trace=0,tab=NULL)
{ 
  p<-dim(W)[3] 
  q<-dim(Z)[3] 
  n<-dim(Y)[3]
  m<-nrow(Y)  

  if(is.null(tab)) 
  {
    theta<-glm( c(Y) ~ -1 + apply(Z,3,c) ,family="poisson" )$coef
    tab<-c(theta,rnorm(p-1)/1e3,rnorm(p)/1e3) 
  }

  fit<-optim(tab,mll_poisblr,
       Y=Y,W=W,X=X,Z=Z,control=list(trace=trace),method="BFGS") 

  tab<-fit$par  
  theta<-tab[1:q] 
  a<-tab[(q+1):(q+p-1)] 
  b<-tab[-(1:(q+p-1))]  

  list(theta=theta,a=a,b=b,tab=tab) 
}


#### ---- fit via alternating reweighted least squares
poisblr_alsfit<-function(Y,W,X,Z,trace=FALSE)
{
  p<-dim(W)[3] 
  q<-dim(Z)[3] 
  n<-dim(Y)[3]
  m<-nrow(Y)

  fit<-glm( c(Y) ~ -1 + apply(Z,3,c) ,family="poisson" ) 
  theta<-fit$coef

  set.seed(1)

  THETA<-theta
  ALPHA<-alpha<-rnorm(p)/n
  BETA<-beta<-rnorm(p)/n
  DEV<-matrix( c(Inf,deviance(fit)) ,1,2)


  #### ---- block coordinate descent 
  while( abs(DEV[nrow(DEV),1]-DEV[nrow(DEV),2])/abs(DEV[nrow(DEV),2]) > 1e-9)
  {

  ## -- update theta, alpha  

  # - construct design matrix
  WSbeta<-amprod(W,t(beta),3)[,,1]
  Wbeta<-array(dim=c(m,m,p,n))
  for(k in 1:p){ Wbeta[,,k,] <- tprod(X,list(W[,,k],WSbeta)) } 
  dimnames(Wbeta)[[3]]<-dimnames(W)[[3]]

  # - conditional fit with glm
  tastart<-c(theta,alpha)
  fit_alpha<-glm(c(Y) ~ -1 + apply(Z,3,c) + apply(Wbeta,3,c),family=poisson,
                         start=tastart,control=list(trace=trace) )
  theta<-fit_alpha$coef[  (1:dim(Z)[3]) ]
  alpha<-fit_alpha$coef[ -(1:dim(Z)[3]) ]


  ## -- update theta, beta

  # - construct design matrix
  WSalpha<-amprod(W,t(alpha),3)[,,1]
  alphaW<-array(dim=c(m,m,p,n))
  for(k in 1:p){ alphaW[,,k,] <- tprod(X,list(WSalpha,W[,,k])) }
  dimnames(alphaW)[[3]]<-dimnames(W)[[3]]

  # - conditional fit with glm
  tbstart<-c(theta,beta)
  fit_beta<-glm(c(Y) ~ -1 + apply(Z,3,c) + apply(alphaW,3,c),family=poisson,
                        start=tbstart, control=list(trace=trace))
  theta<-fit_beta$coef[  (1:dim(Z)[3]) ]
  beta<-fit_beta$coef[ -(1:dim(Z)[3]) ]

  ## -- save results
  ALPHA<-rbind(ALPHA,alpha)
  BETA<-rbind(BETA,beta)
  THETA<-rbind(THETA,theta)
  DEV<-rbind(DEV,c(deviance(fit_alpha),deviance(fit_beta)))

  #cat(date(),DEV[nrow(DEV),],"\n") 
  a<-alpha[-1]/alpha[1] 
  b<-beta*alpha[1] 
  }
  a<-alpha[-1]/alpha[1] 
  b<-beta*alpha[1]  
  list(theta=theta,a=a,b=b,tab=c(theta,a,b)) 
}


#### ---- standard errors based on Hessian
se_poisblr<-function(tab,Y,W,X,Z)
{
  #H<-numDeriv::hessian(mll_poisblr,tab,Y=Y,W=W,X=X,Z=Z) 
  H<-mll_gH(tab,Y,W,X,Z)$hess
  sqrt(diag(solve(H)))
}



poisblr_alsfit_speedglm<-function(Y,W,X,Z,trace=FALSE)
{
  p<-dim(W)[3] 
  q<-dim(Z)[3] 
  n<-dim(Y)[3]
  m<-nrow(Y)

  glmData = data.frame( cbind( Y=c(Y), apply(Z,3,c) ) )
  glmForm = formula(paste0('Y ~ -1 + ', paste(dimnames(Z)[[3]], collapse=' + ')))
  fit <- speedglm(formula=glmForm, data=glmData, family=poisson())
  theta<-fit$coef
  names(theta) = paste0('apply(Z, 3, c)', dimnames(Z)[[3]])
  set.seed(1)

  THETA<-theta
  ALPHA<-alpha<-rnorm(p)/n
  BETA<-beta<-rnorm(p)/n
  DEV<-matrix( c(Inf,deviance(fit)) ,1,2)


  #### ---- block coordinate descent 
  while( abs(DEV[nrow(DEV),1]-DEV[nrow(DEV),2])/abs(DEV[nrow(DEV),2]) > 1e-9)
  {

  ## -- update theta, alpha  

  # - construct design matrix
  WSbeta<-amprod(W,t(beta),3)[,,1]
  Wbeta<-array(dim=c(m,m,p,n))
  for(k in 1:p){ Wbeta[,,k,] <- tprod(X,list(W[,,k],WSbeta)) } 
  dimnames(Wbeta)[[3]]<-dimnames(W)[[3]]

  # - conditional fit with glm
  tastart<-c(theta,alpha)

  glmData = data.frame( cbind(Y=c(Y), apply(Z,3,c), apply(Wbeta,3,c) ) )
  names(glmData)[2:(q+1)] = paste0(names(glmData)[2:(q+1)], '_Z')
  names(glmData)[(q+2):ncol(glmData)] = paste0(names(glmData)[(q+2):ncol(glmData)], '_Wbeta')
  names(glmData) = gsub('.1', '', names(glmData))
  glmForm = formula(paste0('Y ~ -1 + ', paste(names(glmData)[-1], collapse=' + ') ))
  
  fit_alpha<-speedglm(formula=glmForm, data=glmData, family=poisson(),
    start=tastart,control=list(trace=trace) )
  
  theta<-fit_alpha$coef[  (1:dim(Z)[3]) ]
  names(theta) = paste0('apply(Z, 3, c)', dimnames(Z)[[3]])
  alpha<-fit_alpha$coef[ -(1:dim(Z)[3]) ]  
  names(alpha) = paste0('apply(Wbeta, 3, c)', dimnames(W)[[3]])

  ## -- update theta, beta

  # - construct design matrix
  WSalpha<-amprod(W,t(alpha),3)[,,1]
  alphaW<-array(dim=c(m,m,p,n))
  for(k in 1:p){ alphaW[,,k,] <- tprod(X,list(WSalpha,W[,,k])) }
  dimnames(alphaW)[[3]]<-dimnames(W)[[3]]

  # - conditional fit with glm
  tbstart<-c(theta,beta)

  glmData = data.frame( cbind(Y=c(Y), apply(Z,3,c), apply(alphaW,3,c) ) )
  names(glmData)[2:(q+1)] = paste0(names(glmData)[2:(q+1)], '_Z')
  names(glmData)[(q+2):ncol(glmData)] = paste0(names(glmData)[(q+2):ncol(glmData)], '_alphaW')
  names(glmData) = gsub('.1', '', names(glmData))
  glmForm = formula(paste0('Y ~ -1 + ', paste(names(glmData)[-1], collapse=' + ') ))
  
  fit_beta<-speedglm(formula=glmForm, data=glmData, family=poisson(),
    start=tbstart,control=list(trace=trace) )
  
  theta<-fit_beta$coef[  (1:dim(Z)[3]) ]
  names(theta) = paste0('apply(Z, 3, c)', dimnames(Z)[[3]])
  beta<-fit_beta$coef[ -(1:dim(Z)[3]) ]  
  names(beta) = paste0('apply(alphaW, 3, c)', dimnames(W)[[3]])

  ## -- save results
  ALPHA<-rbind(ALPHA,alpha)
  BETA<-rbind(BETA,beta)
  THETA<-rbind(THETA,theta)
  DEV<-rbind(DEV,c(deviance(fit_alpha),deviance(fit_beta)))

  #cat(date(),DEV[nrow(DEV),],"\n") 
  a<-alpha[-1]/alpha[1] 
  b<-beta*alpha[1] 
  }
  a<-alpha[-1]/alpha[1] 
  b<-beta*alpha[1]  
  list(theta=theta,a=a,b=b,tab=c(theta,a,b)) 
}