#################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ source('~/Research/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){ source('~/ProjectsGit/conflictEvolution/R/setup.R')  }

if(Sys.info()['user']=='maxgallop'){ source('~/Documents/conflictEvolution/R/setup.R')  }
#################

#################
ucdp = read.csv(
	url('http://www.pcr.uu.se/digitalAssets/124/124930_1ucdp-non-state-conflict-dataset-v2.5-2015csv.csv'),
	stringsAsFactors=FALSE)

# Subset to mexico
ucdp = ucdp[grepl('Mexico',ucdp$location),]

# Creating time vars
ucdp$ependdate[ucdp$ependdate==''] = NA

breakDate = function(dateVar, pre='start'){
	yearMnth = matrix(
		num(
			unlist(
				lapply(strsplit(dateVar,'-'), function(x){x[1:2]})
				)
			), 
		ncol=2, byrow=TRUE, 
		dimnames=list( NULL, c(paste0(pre, 'Year'), paste0(pre, 'Month')) ) )
	yearMnth = data.frame(yearMnth, stringsAsFactors=FALSE)
	yearMnth$qtr = 1 ; yearMnth$qtr[is.na(yearMnth[,2])] = NA
	yearMnth$qtr[yearMnth[,2]>3 & yearMnth[,2]<7] = 2
	yearMnth$qtr[yearMnth[,2]>6 & yearMnth[,2]<10] = 3
	yearMnth$qtr[yearMnth[,2]>9] = 4
	names(yearMnth)[ncol(yearMnth)] = paste0(pre, 'Qtr')
	return(yearMnth)
}

ucdp = cbind(ucdp, breakDate(ucdp$startdate, pre='start'))
ucdp = cbind(ucdp, breakDate(ucdp$startdate2, pre='start2'))
ucdp = cbind(ucdp, breakDate(ucdp$ependdate, pre='end'))

ucdp = ucdp[ucdp$startYear > 2005 & ucdp$startYear < 2013,]
#################

#################
# Subset to relev actors
panel = read.csv(paste0(pathData,"mexicanActorListMonth.csv"))
panel = panel[!is.na(panel$startYear),]
actorIDs = c('sidea', 'sideb')

# Remove internal conflict cases from tijuana cartel
internalRebels = c('Beltrán Leyva Cartel - Valdez Villareal faction', 'Los Zetas - Velazquez Caballero faction', 'Tijuana Cartel - El Teo faction')
ucdp = ucdp[which(!ucdp$sideb %in% internalRebels),]

uActors = sort( unique( unlist(ucdp[,actorIDs]) ) )
unique(char(panel$lab))[-(1:3)]
for(var in actorIDs){ ucdp[,var] = panel$lab[ match( ucdp[,var], panel$labOld ) ] }
#################
