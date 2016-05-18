##########################################################################
##########################################################################
# 
# REPLICATE TABLES 2 AND 3
# 
# Script for regression results using municipal weekly data
# 
# Osorio (2015), The Contagion of Drug Violence
# 
##########################################################################
##########################################################################



setwd("C:/Users/josorio/Dropbox/spatial analysis RR/R&R/Analysis/replication files/Tables_2_and_3_Regression_Analysis/")
#setwd("D:/DropBox/Dropbox/spatial analysis RR/R&R/Analysis/data/MW data/")


library(foreign)
library(sp)
library(spdep)
library(plm)
library(splm)

# Load data 
data3<-read.csv("data_Lag4.csv")  #TSCS WEEKLY DATA#
data3$rate <- data3$dvd_lag1 - data3$dvd_lag2


W1<-read.dta("W_mun_dummies_base.dta")            #Weights matrix binary#     
#W2<-read.dta("W_mun_dummies_double.dta")          #Weights matrix distance#     

# Create matrix for @
weight1<-data.matrix(W1, rownames.force = NA)
#weight2<-data.matrix(W2, rownames.force = NA)

# create weights using the listw approach (see Anselin's paper)
listw <- read.dta("W_mun_dummies_base.dta")            #Weights matrix binary#  
listw <- as.matrix(listw)
listw <- mat2listw(listw, row.names=NULL)
listw <- nb2listw(listw$neighbours, style="W") 

rm(W1, weight1)


######################################################################################################
#### SPATIAL  MODELS
#### TABLE 2
######################################################################################################


###################################################
## Interaction terms full svd
svd.3 <- dvd_log ~ svd_lag4_log*total_all_dto  + svd_lag4_log*road_dens + drug_production + gulf_3 + north_3 + pacific_3 + indrezsoc +  pob_log + tm_mp  + total_drugs +  rifles_log + coca2010         + potential_cocaine_col  +  escoprom15   + rate
stre_svd.3 <- spgm(formula = svd.3, data = data3, index = "mpal_id", listw = listw, moments = "fullweights", model = "random", spatial.error = TRUE, lag = TRUE)
summary(stre_svd.3)  


###################################################
## Interaction terms full sad
sad.3 <- dvd_log ~ sad_lag4_log*total_all_dto  + sad_lag4_log*road_dens + drug_production + gulf_3 + north_3 + pacific_3 + indrezsoc +  pob_log + tm_mp  + total_drugs +  rifles_log + coca2010         + potential_cocaine_col  +  escoprom15   + rate
stre_sad.3 <- spgm(formula = sad.3, data = data3, index = "mpal_id", listw = listw, moments = "fullweights", model = "random", spatial.error = TRUE, lag = TRUE)
summary(stre_sad.3)  


###################################################
## Interaction terms full ssd
ssd.3 <- dvd_log ~ ssd_lag4_log*total_all_dto  + ssd_lag4_log*road_dens + drug_production + gulf_3 + north_3 + pacific_3 + indrezsoc +  pob_log + tm_mp  + total_drugs +  rifles_log + coca2010         + potential_cocaine_col  +  escoprom15   + rate
stre_ssd.3 <- spgm(formula = ssd.3, data = data3, index = "mpal_id", listw = listw, moments = "fullweights", model = "random", spatial.error = TRUE, lag = TRUE)
summary(stre_ssd.3)  


###################################################
## Interaction terms full ssa
ssa.3 <- dvd_log ~ ssa_lag4_log*total_all_dto  + ssa_lag4_log*road_dens + drug_production + gulf_3 + north_3 + pacific_3 + indrezsoc +  pob_log + tm_mp  + total_drugs +  rifles_log +                   + potential_cocaine_col  +  escoprom15   + rate
stre_ssa.3 <- spgm(formula = ssa.3, data = data3, index = "mpal_id", listw = listw, moments = "fullweights", model = "random", spatial.error = TRUE, lag = TRUE)
summary(stre_ssa.3)  


###################################################
## Interaction terms full ssg
ssg.3 <- dvd_log ~ ssg_lag4_log*total_all_dto  + ssg_lag4_log*road_dens + drug_production + gulf_3 + north_3 + pacific_3 + indrezsoc +  pob_log + tm_mp  + total_drugs +  rifles_log                     + potential_cocaine_col  +  escoprom15   + rate
stre_ssg.3 <- spgm(formula = ssg.3, data = data3, index = "mpal_id", listw = listw, moments = "fullweights", model = "random", spatial.error = TRUE, lag = TRUE)
summary(stre_ssg.3)  






######################################################################################################
#### SPATIAL  MODELS
#### TABLE 3
######################################################################################################


###################################################
## Interaction terms full svd
svd.3 <- dvd_log ~ svd_lag4_log*total_main_dto + svd_lag4_log*total_other_dto  + svd_lag4_log*road_dens + drug_production + gulf_3 + north_3 + pacific_3 + indrezsoc + pob_log                        +  rifles_log             + potential_cocaine_col  +  escoprom15 + rate
stre_svd.3 <- spgm(formula = svd.3, data = data3, index = "mpal_id", listw = listw, moments = "fullweights", model = "random", spatial.error = TRUE, lag = TRUE)
summary(stre_svd.3)  


###################################################
# Interaction terms full sad
sad.3 <- dvd_log ~ sad_lag4_log*total_main_dto + sad_lag4_log*total_other_dto  + sad_lag4_log*road_dens + drug_production + gulf_3 + north_3 + pacific_3 + indrezsoc + pob_log                        +  rifles_log             + potential_cocaine_col  +  escoprom15 + rate
stre_sad.3 <- spgm(formula = sad.3, data = data3, index = "mpal_id", listw = listw, moments = "fullweights", model = "random", spatial.error = TRUE, lag = TRUE)
summary(stre_sad.3)  


###################################################
## Interaction terms full ssd
ssd.3 <- dvd_log ~ ssd_lag4_log*total_main_dto + ssd_lag4_log*total_other_dto  + ssd_lag4_log*road_dens + drug_production + gulf_3 + north_3 + pacific_3 + indrezsoc + pob_log                        +  rifles_log             + potential_cocaine_col  +  escoprom15 + rate
stre_ssd.3 <- spgm(formula = ssd.3, data = data3, index = "mpal_id", listw = listw, moments = "fullweights", model = "random", spatial.error = TRUE, lag = TRUE)
summary(stre_ssd.3)  


###################################################
## Interaction terms full ssa
ssa.3 <- dvd_log ~ ssa_lag4_log*total_main_dto + ssa_lag4_log*total_other_dto  + ssa_lag4_log*road_dens + drug_production + gulf_3 + north_3 + pacific_3 + indrezsoc + pob_log                        +  rifles_log             + potential_cocaine_col  +  escoprom15 + rate
stre_ssa.3 <- spgm(formula = ssa.3, data = data3, index = "mpal_id", listw = listw, moments = "fullweights", model = "random", spatial.error = TRUE, lag = TRUE)
summary(stre_ssa.3)  


###################################################
# Interaction terms full ssg
ssg.3 <- dvd_log ~ ssg_lag4_log*total_main_dto + ssg_lag4_log*total_other_dto  + ssg_lag4_log*road_dens + drug_production + gulf_3 + north_3 + pacific_3 + indrezsoc + pob_log                         +  rifles_log             + potential_cocaine_col  +  escoprom15 + rate
stre_ssg.3 <- spgm(formula = ssg.3, data = data3, index = "mpal_id", listw = listw, moments = "fullweights", model = "random", spatial.error = TRUE, lag = TRUE)
summary(stre_ssg.3)  





