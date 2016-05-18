##########################################################################
##########################################################################
# 
# REPLICATE TABLE 1 AND FIGURE 5
# 
# Script for calculating Global and Local Moran's I using municipal rearly data
# 
# Osorio (2015), The Contagion of Drug Violence
# 
##########################################################################
##########################################################################



##########################################################################
# load libraries
library(foreign)
library(splm)
library(spdep)
library(spam)
library(ape)

##########################################################################

# Setworking directory
#setwd("D:/DropBox/Dropbox/spatial analysis narco/Analysis/R_scripts/R&R/T1_F5_Moran_I")
setwd("D:/DropBox/Dropbox/spatial analysis RR/R&R/Analysis/replication files/T1_F5_Moran_I")


##########################################################################
##########################################################################
# REPLICATE TABLE 1 
# Global Moran's I
##########################################################################
##########################################################################


##########################################################################
# Load data 

data_0010<-read.dta("OCVED_MY_0010.dta")

data_10<-subset(data_0010, year==2010, select=c(year_mun, year, mpal_id, DVD))
data_09<-subset(data_0010, year==2009, select=c(year_mun, year, mpal_id, DVD))
data_08<-subset(data_0010, year==2008, select=c(year_mun, year, mpal_id, DVD))
data_07<-subset(data_0010, year==2007, select=c(year_mun, year, mpal_id, DVD))
data_06<-subset(data_0010, year==2006, select=c(year_mun, year, mpal_id, DVD))
data_05<-subset(data_0010, year==2005, select=c(year_mun, year, mpal_id, DVD))
data_04<-subset(data_0010, year==2004, select=c(year_mun, year, mpal_id, DVD))
data_03<-subset(data_0010, year==2003, select=c(year_mun, year, mpal_id, DVD))
data_02<-subset(data_0010, year==2002, select=c(year_mun, year, mpal_id, DVD))
data_01<-subset(data_0010, year==2001, select=c(year_mun, year, mpal_id, DVD))
data_00<-subset(data_0010, year==2000, select=c(year_mun, year, mpal_id, DVD))



## Spatial matrix

#W1<-read.dta("W_mun_dummies_base.dta")            #Weights matrix binary#     
W2<-read.dta("W_mun_dummies_double.dta")          #Weights matrix distance#     

# Create matrix for @
#weight1<-data.matrix(W1, rownames.force = NA)
weight2<-data.matrix(W2, rownames.force = NA)


# create weights using the listw approach (see Anselin's paper)
listw <- read.dta("W_mun_dummies_base.dta")            #Weights matrix binary#  
listw <- as.matrix(listw)
listw <- mat2listw(listw, row.names=NULL)
listw <- nb2listw(listw$neighbours, style="W") 


##########################################################################
##########################################################################
# Global Moran's I
#####
moran_10 <- Moran.I(data_10$DVD,weight2)
moran_09 <- Moran.I(data_09$DVD,weight2)
moran_08 <- Moran.I(data_08$DVD,weight2)
moran_07 <- Moran.I(data_07$DVD,weight2)
moran_06 <- Moran.I(data_06$DVD,weight2)
moran_05 <- Moran.I(data_05$DVD,weight2)
moran_04 <- Moran.I(data_04$DVD,weight2)
moran_03 <- Moran.I(data_03$DVD,weight2)
moran_02 <- Moran.I(data_02$DVD,weight2)
moran_01 <- Moran.I(data_01$DVD,weight2)
moran_00 <- Moran.I(data_00$DVD,weight2)

# Generate matrix of results
moran.matrix <-matrix(c(moran_00, moran_01, moran_02, moran_03, moran_04, moran_05, moran_06, moran_07, moran_08, moran_09, moran_10),ncol=11)
colnames(moran.matrix) <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
rownames(moran.matrix) <- c("Observed", "Expected", "sd","p.value")

# Print matrix of results
print(t(moran.matrix))





##########################################################################
##########################################################################
# REPLICATE FIGURE 5 
# Local Moran's I
##########################################################################
##########################################################################



##########################################################################
# Local Moran's I
#####
localmoran_10 <- localmoran(data_10$DVD,listw,p.adjust.method="bonferroni")
local_10<-data.frame(localmoran_10)
localmoran_09 <- localmoran(data_09$DVD,listw,p.adjust.method="bonferroni")
local_09<-data.frame(localmoran_09)
localmoran_08 <- localmoran(data_08$DVD,listw,p.adjust.method="bonferroni")
local_08<-data.frame(localmoran_08)
localmoran_07 <- localmoran(data_07$DVD,listw,p.adjust.method="bonferroni")
local_07<-data.frame(localmoran_07)
localmoran_06 <- localmoran(data_06$DVD,listw,p.adjust.method="bonferroni")
local_06<-data.frame(localmoran_06)
localmoran_05 <- localmoran(data_05$DVD,listw,p.adjust.method="bonferroni")
local_05<-data.frame(localmoran_05)
localmoran_04 <- localmoran(data_04$DVD,listw,p.adjust.method="bonferroni")
local_04<-data.frame(localmoran_04)
localmoran_03 <- localmoran(data_03$DVD,listw,p.adjust.method="bonferroni")
local_03<-data.frame(localmoran_03)
localmoran_02 <- localmoran(data_02$DVD,listw,p.adjust.method="bonferroni")
local_02<-data.frame(localmoran_02)
localmoran_01 <- localmoran(data_01$DVD,listw,p.adjust.method="bonferroni")
local_01<-data.frame(localmoran_01)
localmoran_00 <- localmoran(data_00$DVD,listw,p.adjust.method="bonferroni")
local_00<-data.frame(localmoran_00)






##################################################
# Histograms of significant local Morans I

s_10 = subset(local_10,Pr.z...0.<0.1)
s_09 = subset(local_09,Pr.z...0.<0.1)
s_08 = subset(local_08,Pr.z...0.<0.1)
s_07 = subset(local_07,Pr.z...0.<0.1)
s_06 = subset(local_06,Pr.z...0.<0.1)
s_05 = subset(local_05,Pr.z...0.<0.1)
s_04 = subset(local_04,Pr.z...0.<0.1)
s_03 = subset(local_03,Pr.z...0.<0.1)
s_02 = subset(local_02,Pr.z...0.<0.1)
s_01 = subset(local_01,Pr.z...0.<0.1)
s_00 = subset(local_00,Pr.z...0.<0.1)



##############################################
# Outliers in local spatial autocorrelation

pdf("local_outliers_box.pdf", width=7, height=4)
boxplot(s_00$Z.Ii,
        s_01$Z.Ii,
        s_02$Z.Ii,
        s_03$Z.Ii,
        s_04$Z.Ii,
        s_05$Z.Ii,
        s_06$Z.Ii,       
        s_07$Z.Ii,  
        s_08$Z.Ii,  
        s_09$Z.Ii,  
        s_10$Z.Ii,
        ylab="Z-scores",
        xlabels=NULL
)
labels <- paste(c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010"))
axis(1, at=1:11, par(las=2), labels=labels)
dev.off()


