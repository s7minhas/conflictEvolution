#################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
#################

#################
load(paste0(pathData, 'mexicoVioSqlStories0116.Rda'))
names(mexicoStories)[2:3] = c('sender','target')

actors = unique( c( mexicoStories$sender, mexicoStories$target ) )
actors = data.frame(actor = actors, stringsAsFactors = FALSE)
write.csv(actors, file=paste0(pathData, 'actorList0116.csv'))
#################

#################
actors = read.csv(paste0(pathData, 'actorList0116MS.csv'))
mexActors = char( actors$X[which(actors$notMexConflict==0)] )

mexicoStoriesSubset = mexicoStories[which(mexicoStories$sender %in% mexActors),]
mexicoStoriesSubset = mexicoStoriesSubset[which(mexicoStoriesSubset$target %in% mexActors),]
#################

#################
mexicoStoriesSubset$year = num(unlist(lapply(strsplit(mexicoStoriesSubset$event_date, '-'), function(x){x[1]})))
mexicoStoriesSubset = mexicoStoriesSubset[mexicoStoriesSubset$year>2005 & mexicoStoriesSubset$year<2013,]
#################

#################
# save revised story db
save(mexicoStoriesSubset, file=paste0(pathData, 'mexicoVioSqlStories_onlyMexActors_2006_2012.Rda'))
#################