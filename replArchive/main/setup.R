####################################
# Clear workspace
rm(list=ls())
####################################

####################################
# Load helpful libraries
loadPkg=function(toLoad){
    for(lib in toLoad){
      if(!(lib %in% installed.packages()[,1])){ 
        install.packages(lib, repos='http://cran.rstudio.com/') }
      suppressMessages( library(lib, character.only=TRUE) ) } }

toLoad = c(
	'foreach', 'doParallel', 
	'network', 'igraph', 'ggplot2', 'RColorBrewer',
	'reshape2', 'plyr', 'magrittr', 'latex2exp', 
	'amen', 'xtable'
	)
loadPkg(toLoad)

## gg theme
theme_set(theme_bw())
####################################

####################################
# Helpful functions
pasteVec = function(x,y){ as.vector( outer( x, y, paste0 ) ) }
char = function(x) { as.character(x) }
num = function(x) { as.numeric(char(x)) }
cname = function(x) { countrycode(x, 'country.name', 'country.name') }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
substrRight = function(x, n){ substr(x, nchar(x)-n+1, nchar(x)) }
summStats = function(x){
	mu=mean(x)
	qts=quantile(x, probs=c(0.025, 0.975, 0.05, 0.95))
	return( c(mu, qts) ) }
# https://cran.r-project.org/doc/contrib/Lemon-kickstart/rescale.R
rescale<-function(x,newrange) {
 if(nargs() > 1 && is.numeric(x) && is.numeric(newrange)) {
  # if newrange has max first, reverse it
  if(newrange[1] > newrange[2]) {
   newmin<-newrange[2]
   newrange[2]<-newrange[1]
   newrange[1]<-newmin
  }
  xrange<-range(x)
  if(xrange[1] == xrange[2]) stop("can't rescale a constant vector!")
  mfac<-(newrange[2]-newrange[1])/(xrange[2]-xrange[1])
  invisible(newrange[1]+(x-xrange[1])*mfac)
 }
 else {
  cat("Usage: rescale(x,newrange)\n")
  cat("\twhere x is a numeric object and newrange is the min and max of the new range\n")
 }
}	