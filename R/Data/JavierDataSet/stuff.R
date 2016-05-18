
rm(list=ls())
if(Sys.info()["user"]=="cassydorff")
  { setwd("~/ProjectsGit/conflictEvolution/R/Data/Dataset/Replication_files/") }
  
# load libraries
library(foreign)
library(splm)
library(spdep)
library(spam)
library(ape)

data_0010<-read.dta("Table_1_and_Figure_5_Moran_I/OCVED_MY_0010.dta")
W2<-read.dta("Table_1_and_Figure_5_Moran_I/W_mun_dummies_double.dta")
data3<-read.csv("Appendix_Tables_10_to_15/data_Lag4.csv")  #TSCS WEEKLY DATA#