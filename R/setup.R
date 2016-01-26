# Clear workspace
rm(list=ls())

####################################
# Set up paths
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	dpth='~/Dropbox/Research/conflictEvolution/';
	gpth='~/Research/conflictEvolution/';
	pathData=paste0(dpth, 'data/');
	pathGraphics=paste0(dpth, 'graphics/')
}

if(Sys.info()["user"]=="cassydorff"){
	dpth='~/Dropbox/Research/conflictEvolution/'
	gpth='~/ProjectsGit/conflictEvolution/'
	pathData=paste0(dpth, 'data/');
	pathGraphics=paste0(dpth, 'graphics/')
}
####################################

####################################
# Load helpful libraries
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  library(lib, character.only=TRUE)
	} }

toLoad=c(
	'network','amen', 'MASS', 'ggplot2',
	'plyr', 'xtable', 'abind'
	)

loadPkg(toLoad)

## gg theme
theme_set(theme_bw())
####################################

####################################
# Helpful functions
char = function(x) { as.character(x) }
num = function(x) { as.numeric(char(x)) }
cname = function(x) { countrycode(x, 'country.name', 'country.name') }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
substrRight = function(x, n){ substr(x, nchar(x)-n+1, nchar(x)) }
####################################