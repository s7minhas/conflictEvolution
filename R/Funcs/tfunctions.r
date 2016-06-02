
#' normal scores
#' 
#' Compute normal scores
#'
#' This function applies a quantile-quantile transformation to 
#' the data, resulting in a distribution that is approximately normal
#' but has the same ranks as the original data
#' 
#' @param y a vector 
#' @author Peter Hoff
#' @keywords transformation
#' @export
#' @examples
#' y<-rexp(100)
#' z<-zscores(y) 
#' par(mfrow=c(1,3))
#' hist(y) 
#' hist(z)
#' plot(y,z) 
zscores<-function(y)
{
  qnorm(rank(y,na.last="keep")/(sum(!is.na(y))+1) )
}


#' Kronecker sum
#'
#' Calculate a kronecker product 
#' 
#' This function calculates the Kronecker sum of a list 
#' of matrices.  
#' 
#' @param \ldots Either a sequence of matrices, or a single list object of matrices 
#' @author Peter Hoff 
#' @keywords matrices 
#' @export
#' @examples
#' A<-rsan(c(2,3)) ; B<-rsan(c(3,4)) ; C<-rsan(c(4,5))
#' ksum(A,B,C) 
#' M<-list(A,B,C)
#' ksum(M) 
ksum<-function(...)
{
  M<-list(...)
  if(is.list(M[[1]])) { M<-M[[1]] }
  KM<-1 ; for(k in 1:length(M)){ KM<-kronecker(KM,exp(M[[k]])) }
  log(KM)
}


#' Tucker sum
tsum<-function(X,A)
{
  ### needs work
  m<-sapply(A,function(x){dim(x)[1] } )
  K<-length(m)
  XA<-array(0,dim=c(m,dim(X)[-(1:length(m))]))

  for(k in 1:K)
  {
    XA<-sweep(XA, c(k,K+1), A[[k]]%*%apply(X,c(k,K+1),sum),"+" )
  }
 XA
}
###



#' Kronecker product
#'
#' Calculate a kronecker product 
#' 
#' This function calculates the Kronecker product of a list 
#' of matrices.  
#' 
#' @param \ldots Either a sequence of matrices, or a single list object of matrices 
#' @author Peter Hoff 
#' @keywords matrices 
#' @export
#' @examples
#' A<-rsan(c(2,3)) ; B<-rsan(c(3,4)) ; C<-rsan(c(4,5))
#' kron(A,B,C) 
#' M<-list(A,B,C)
#' kron(M) 
kron<-function(...)
{
  M<-list(...)
  if(is.list(M[[1]])) { M<-M[[1]] }
  KM<-1 ; for(k in 1:length(M)){ KM<-kronecker(KM,M[[k]]) }
  KM
}



#' Wishart simulation
#'
#' Simulate a Wishart-distributed random matrix
#'
#' This function simulates a Wishart random matrix
#' using Bartletts decomposition, as described in Everson and 
#' Morris (2000).
#'
#' @param S0 a positive definite matrix. 
#' @param nu a positive scalar 
#' @author Peter Hoff
#' @keywords multivariate simulation
#' @export
#' @examples
#' # simulate several matrices and compute the mean
#' SS<-matrix(0,5,5)
#' for(s in 1:1000) { SS<-SS+rwish(diag(5),3) } 
#' SS/s  
rwish<-function(S0,nu=dim(as.matrix(S0))[1]+1)
{
  S0<-as.matrix(S0)
  S0h<-eigen(S0)
  S0h<-S0h$vec%*%diag(sqrt(S0h$val),nrow=length(S0h$val))%*%t(S0h$vec)

  p<-dim(S0)[1]
  T<-matrix(0,p,p)
  T[lower.tri(T)]<-rnorm( p*(p-1)/2)
  diag(T)<-sqrt( rgamma(p,(nu-(1:p)+1)/2,1/2) )
  S0h%*%T%*%t(T)%*%S0h
}


#' Multivariate normal simulation
#'
#' Simulate a multivariate normal random matrix
#'
#' This function simulates multivariate normal random vectors
#'
#' @param n number of mvnormal vectors to simulate
#' @param mu mean vector
#' @param Sigma covariance matrix
#' @param Sigma.chol Cholesky decomposition of \code{Sigma}
#' @author Peter Hoff
#' @keywords multivariate simulation
#' @export
#' @examples
#' # simulate several matrices and compute the mean
#' Y<-rmvnorm(100,c(1,2,3),matrix(c(3,0,1,0,1,-1,1,-1,2),3,3))
#' colMeans(Y) 
#' cov(Y) 
rmvnorm<-function(n,mu,Sigma,Sigma.chol=chol(Sigma))
{
  E<-matrix(rnorm(n*length(mu)),n,length(mu))
  X<-t(  t(E%*%Sigma.chol) +c(mu))
  if(n==1) { X<-c(X)}
  X
}



#' Matricization
#' 
#' Matricize an array
#'
#' This functions matricizes an array along a given mode. 
#'
#' @param A an array
#' @param k a mode of \code{A}, along which to matricize
#' @keywords arrays matrices
#' @export
#' @examples
#' A<-rsan(4,3,2)
#' mat(A,2) 
mat<-function(A,k)
{
  Ak<-t(apply(A,k,"c"))
  if(nrow(Ak)!=dim(A)[k])  { Ak<-t(Ak) }
  Ak
}

#' Array-matrix product
#'
#' Multiply an array by a matrix along a given mode
#'
#' This function multiplies a matricized array by another 
#' matrix, and then reforms the result into a new array. 
#'
#' @param A a real valued array 
#' @param M a real matrix
#' @param k an integer, a mode of \code{A}
#' @author Peter Hoff
#' @keywords arrays
#' @export
#' @examples
#' A<-rsan(c(5,4,3))
#' B<-rsan(c(2,5))
#' amprod(A,B,1)
amprod<-function(A,M,k)
{
  K<-length(dim(A))
  AM<-M%*%mat(A,k)
  AMA<-array(AM, dim=c(dim(M)[1],dim(A)[-k]) )
  aperm(AMA,  match(1:K,c(k,(1:K)[-k]) ) )
}


#' Tucker product
#'
#' Multiply an array by a list of matrices along each mode
#'
#' This function multiplies an array along each mode.
#'
#' @param A a real valued array 
#' @param B a list of matrices, the second dimension of each matching the 
#' dimensions of A 
#' @param modes a vector giving which modes of A should be multiplied 
#' @author Peter Hoff
#' @keywords arrays
#' @export
#' @examples
#' m<-c(6,5,4)
#' A<-rsan(c(6,5,4)) 
#' B<-list() ; for(k in 1:3) { B[[k]]<-rsan(c(3,dim(A)[[k]])) } 
tprod<-function(A,B,modes=1:length(B))
{
  X<-A
  for(k in modes) { X<-amprod(X,B[[k]],k) }
  X
}



#' Standard normal array
#' 
#' Generate an array of iid standard normal variables
#' 
#' This functions generates an array of dimension \code{dim}
#' filled with iid standard normal variables. 
#' 
#' @param dim a vector of positive integers
#' @author Peter Hoff
#' @keywords simulation multivariate
#' @export
#' @examples
#' rsan(c(5,4,3))
rsan<-function(dim)
{
  array(rnorm(prod(dim)),dim)
}




#' Symmetric square root of a matrix
#' 
#' Compute the symmetric square root of a matrix
#' 
#' This functions computes the symmetric square root 
#' of a postive semidefinite matrix.
#' 
#' @param M a positive semidefinite matrix
#' @author Peter Hoff
#' @keywords matrices
#' @export
#' @examples
#' S<-rwish(diag(5))
#' Sh<-mhalf(S)
#' Sh%*%Sh 
mhalf<-function(M)
{
  tmp<-eigen(M)
  tmp$vec%*%sqrt(diag(tmp$val,nrow=nrow(M)))%*%t(tmp$vec)
}


#' Matrix trace
#' 
#' Compute the trace of a matrix 
#'
#' This function computes the trace of a square matrix. 
#'
#' @param A a square matrix
#' @author Peter Hoff
#' @keywords matrices
#' @export 
#' @examples
#' tr(riwsh(diag(5)))
tr<-function(A)
{
  sum(diag(A))
}


#' Kronecker product
#'
#' Calculate a kronecker product
#' 
#' This function calculates the Kronecker product of a list 
#' of matrices.  
#' 
#' @param \ldots Either a sequence of matrices, or a single list object of matrices
#' @author Peter Hoff 
#' @keywords matrices
#' @export
#' @examples
#' A<-rsan(c(2,3)) ; B<-rsan(c(3,4)) ; C<-rsan(c(4,5))
#' kron(A,B,C) 
#' M<-list(A,B,C)
#' kron(M) 
kron<-function(...)
{
  M<-list(...)
  if(is.list(M[[1]])) { M<-M[[1]] }
  KM<-1 ; for(k in 1:length(M)){ KM<-kronecker(KM,M[[k]]) }
  KM
}




