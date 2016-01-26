################################################################
## analysis paper 3 - 
## clean SQL data - pre data collectin
## Cassy Dorff
################################################################

# setup & load all data
setwd("/Users/cassydorff/Dropbox/Research/Dissertation/Paper 3/analysis/")  
library(plyr)
library(ggplot2)
library(xtable)

# violence data from original sql
# all events with conflictual actors, huge data set
load(mexico1, file="mexicoVioSqlData.Rda") 

# protest data from original sql
# all protests data
load(mexico.prot, file="mexicoprot.rda")

# cleaned and recoded stories by Cassy
cleanData<-read.csv("data/mexicoVioSqlStoriesCopy.csv", header=TRUE)

# subset for 2004-2010
# fix date format
cleanData$event_dateCh<-as.character(as.Date(cleanData$event_date, format="%d-%m-%Y"))
cleanData$monthYear <- strftime(cleanData$event_dateCh, format="%Y-%m")
cleanData$Year <- strftime(cleanData$event_dateCh, format="%Y")

cleanData<-cleanData[cleanData$event_dateCh>="2004-03-18" & 
                       cleanData$event_dateCh<="2010-03-18",]
range(cleanData$event_dateCh)

# other subsets
originalData<-cleanData[cleanData$event_id > 716,]  #273 obs
originalData<-originalData[originalData$drop==0,] #187
cleanData<-cleanData[cleanData$drop==0,] #387 before, 292 after, added 105 cases
dim(originalData)
dim(cleanData)

# summary & descriptives
table(originalData$code)
length(which(originalData$name_ta=="Criminal (Mexico)"))
length(which(originalData$name_ta=="Armed Gang (Mexico)"))
length(which(originalData$name_ta=="Armed Rebel (Mexico)"))

xtable(table(originalData$name_sa))
xtable(table(cleanData$name_ta_cd))
length(which(originalData$name_sa=="Armed Gang (Mexico)"))

## 
## plots prep & plots
##

# create counter
cleanData$eventfoo <-1
cleanDataCount<-ddply(cleanData, .(monthYear), summarise, eventCount=sum(eventfoo))
cleanDataCount$monthYear<-as.Date(paste0(cleanDataCount$monthYear, "-01"), format="%Y-%m-%d")
cleanDataCount$Year <- strftime(as.Date(cleanDataCount$monthYear), format="%Y")

# or make a whole separate data frame
cleanDataCountYear<-ddply(cleanData, .(Year), summarise, eventCount=sum(eventfoo))

dev.new(height=4, width=8)
q<-qplot(x=monthYear, y=eventCount, data=cleanDataCount[cleanDataCount$Year>=2004
                            & cleanDataCount$Year<2010,], geom="line", group=1)
q
dev.off()


# now do the same using original data
originalData$eventfoo<-1

# create counter
originalDataCount<-ddply(originalData, .(monthYear), summarise, eventCount=sum(eventfoo))
originalDataCount$monthYear<-as.Date(paste0(originalDataCount$monthYear, "-01"), format="%Y-%m-%d")
originalDataCount$Year <- strftime(as.Date(originalDataCount$monthYear), format="%Y")

# or make a whole separate data frame
originalDataCountYear<-ddply(originalData, .(Year), summarise, eventCount=sum(eventfoo))

# plots
qplot(x=monthYear, y=eventCount, data=originalDataCount[originalDataCount$Year>=2004
                      & originalDataCount$Year<2010,], geom="line", group=1)

# both subsets
sub1<-cleanDataCount[cleanDataCount$Year>=2004
                     & cleanDataCount$Year<2010,]
sub2<-originalDataCount[originalDataCount$Year>=2004
                     & originalDataCount$Year<2010,]

dev.new(height=4, width=8)
plot(sub1$monthYear, sub1$eventCount, type="l", lwd=3, bty="n", col="coral", 
                    ylim=c(0,30),
                    xlab="Years", ylab="Event Count", main="Conflictual Events Over Time")
lines(sub2$monthYear, sub2$eventCount, type="l", lwd=3, col="grey64")
dev.off()

#save cleaned data, 2004-2010 for network analysis
save(cleanData, file="cleanData.rda")
save(originalData, file="originalData.rda")

################################################################
## analysis of other data for overtime plots & counts
## using data from original sqls
################################################################

# shorten by year if needed
allCases<-mexico1[mexico1$event_date>="2004-01-02" & mexico1$eventDate<="2010-01-01",]

# select by event type
allCases<-allCases[allCases$code==c("180", "181", "182","1821","1822","1823","183",
                                    "184","185","186", "190","191","192","193","194",
                                    "195","202","1213"),]
dim(allCases)

# fix date format
allCases$monthYear <- strftime(allCases$event_date, format="%Y-%m")
allCases$Year <- strftime(allCases$event_date, format="%Y")

# create counter
allCases$eventfoo <-1
allCasesCount<-ddply(allCases, .(monthYear), summarise, eventCount=sum(eventfoo))
allCasesCount$monthYear<-as.Date(paste0(allCasesCount$monthYear, "-01"), 
                                 format="%Y-%m-%d")

allCasesCount$Year <- strftime(as.Date(allCasesCount$monthYear), format="%Y")

# or make a whole separate data frame
allCasesCountYear<-ddply(allCases, .(Year), summarise, eventCount=sum(eventfoo))

# make combined data frame 
allCasesCount$date<-allCasesCount$monthYear
mexico.prot$Year <- strftime(as.Date(mexico.prot$date), format="%Y")
foo<-merge(mexico.prot, allCasesCount, by="date", all.x=TRUE)
foo[is.na(foo$eventCount),]$eventCount<- 0

##
## Plots
##

# plot both protest and conflict events, monthly
ggplot(foo, aes(date)) + 
  geom_line(aes(y = protest_tALL, colour = "Protest")) + 
  geom_line(aes(y = eventCount, colour = "Conflict"))

# plot only conflict events, monthly
dev.new(height=4, width=8)
qplot(x=date, y=eventCount, data=foo, geom="line", group=1)
dev.off()

# plot only protest events, monthly
dev.new(height=4, width=8)
qplot(x=date, y=protest_tALL, data=foo, geom="line", group=1)
dev.off()
