##
##    Create aggregate counts of events by country-month
##  	Protest counts Mexico 2001 on
##    Create data of conflictual events between armed actors
##    Andy edits
##

#setwd("/Users/maxgallop/Documents/conflictevolution/r/data/old") 
#source("mysqlSetup.R")

setwd("/Users/cassydorff/ProjectsGit/conflictevolution/r/data/old") 
source("mysqlSetup.R")

mysqlSetup(user="cassydorff", pw="5588orange")  # fill in
mysqlSetup(user="maxg", pw="blue87lake")  # fill in

library(ggmap)
library("RMySQL")
library("lubridate")

# with raw.text (see AndySQL.R for without rawtext)
# the first time was start date 2004

sql1<-
  "SELECT s.event_id, sa.name, ta.name, s.event_date, 
t.code, t.goldstein, c.cowcode, st.rawtext
FROM   simple_events s
JOIN   eventtypes t USING(eventtype_id)
JOIN   locations l USING(location_id)
JOIN   countries c ON c.id = l.country_id
JOIN   stories st ON st.StoryID = s.story_id
JOIN   dict_actors sa ON sa.actor_id = s.source_actor_id
JOIN   dict_actors ta ON ta.actor_id = s.target_actor_id
WHERE  s.event_date > '2000-12-31'   # events are shaky before ~ 2000
AND  c.countryname = 'Mexico'
AND  (s.target_actor_id IN
(SELECT dsm.actor_id
FROM   dict_sector_mappings dsm
JOIN   dict_sectors ds USING(sector_id)
WHERE  code IN ('REB', 'INS', 'SEP', 'COP', 'GOV', 
'SEP', 'MIL') 

OR description IN ('Insurgents', 
'Organized Violent', 'Rebel', 
'Radicals / Extremists / Fundamentalists',
'Separatists', 'Terrorists')
)

OR 
s.source_actor_id IN
(SELECT dsm.actor_id
FROM   dict_sector_mappings dsm
JOIN   dict_sectors ds USING(sector_id)
WHERE  code IN ('REB', 'INS', 'SEP', 'COP', 'GOV', 'SEP', 'MIL') 

OR description IN ('Insurgents', 'Organized Violent', 'Rebel', 
'Radicals / Extremists / Fundamentalists',
'Separatists', 'Terrorists')
)
)
;
"

# Get data
mexico1 <- dbGetQuery(conn, sql1)

dim(mexico1)
head(mexico1)
names(mexico1)

# save original
# save(mexico1, file="mexicoVioSqlData.Rda") #start date "2004-01-01"
# write.csv(mexico1, file="mexicoVioSqlData.csv") #start date "2004-01-01"

# shorten by year if needed
drugWar<-mexico1[mexico1$event_date>="2005-01-02",]
dim(drugWar)

# select by event type
allCases<-drugWar[drugWar$code %in% c("180", "181", "182","1821","1822","1823","183","184",
                                    "185","186", "190","191","192","193","194","195","202","1213"),]
dim(allCases)

mexicoStories<-mexico1[mexico1$code %in% c("180", "181", "182","1821","1822","1823","183","184",
                                   "185","186", "190","191","192","193","194","195","202","1213"),]

save(mexicoStories, file="mexicoVioSqlStories0116.Rda") #start date "2004-01-01" with 539 obs
write.csv(mexicoStories, file="mexicoVioSqlStories0116.csv") #start date "2004-01-01"


drugWar<-drugWar[drugWar$code %in% c("180", "181", "182","1821","1822","1823","183","184",
                                 "185","186", "190","191","192","193","194","195","202","1213"),]
dim(allCases)
dim(drugWar)
