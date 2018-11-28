################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }

# load fns from helper file
source(paste0(fPth, 'netPlotHelpers.R'))
source(paste0(fPth, 'actorInfo.R'))
################

################
# load data
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
load(paste0(pathData, 'exoVars.rda')) # load xNodeL, xDyadL

# focus on post 2000 data [few actors beforehand]
yrs = char(2000:2016)
yList = yList[yrs] ; xDyadL = xDyadL[yrs] ; xNodeL = xNodeL[yrs]
###############

###############
# get count of civ vic over time
vioCivCnt = data.frame(cnt=unlist(lapply(xNodeL, function(x){ sum(x[,'vioCivEvents']) })),row.names=NULL)
vioCivCnt$year = num(yrs)

# get density of network over time
vioCivCnt$dens = unlist(lapply(yList, function(x){mean(c(x))}))

# melt
vioCivCnt = melt(vioCivCnt, id='year')
vioCivCnt$variable = char(vioCivCnt$variable)
vioCivCnt$variable[vioCivCnt$variable=='cnt'] = 'Number of Violent Events Against Civilians'
vioCivCnt$variable[vioCivCnt$variable=='dens'] = 'Network Density'

ggVioCiv = ggplot(vioCivCnt, aes(x=year, y=value)) +
	geom_line() +
	facet_wrap(~variable, scales='free_y', nrow=2) +
	xlab('') + ylab('') +
	theme(
		axis.ticks=element_blank(),
		panel.border=element_blank(), 
		axis.text=element_text(family="Source Code Pro Light",size=8),
		strip.text.x = element_text(size = 10, color='white',
			family="Source Code Pro Semibold", angle=0, hjust=.05),
		strip.background = element_rect(fill = "#525252", color='#525252')		
		)	
ggsave(ggVioCiv, 
	width=10, height=5, 
	file=paste0(pathGraphics, 'civVicConfNet/vioCiv.pdf'), device=cairo_pdf)
###############

################
# clean up row/col names in yList
vNameKey = getNameKey(yList)
yList = lapply(yList, function(y){
	rownames(y) = vNameKey$clean[match(rownames(y),vNameKey$dirty)]
	colnames(y) = vNameKey$clean[match(colnames(y),vNameKey$dirty)]
	return(y) })

# set up actor positions across T
yArr = listToArray(actors=getActor(yList), Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
yArrSum = getMatfromArr(arr=yArr)
govActors = c('Military\n(Nigeria)','Police\n(Nigeria)')

# set up pos mat
tmp=getGraphfromMat(mat=yArrSum) ; gArrSum=tmp$g ; gArrPos=tmp$nodePos ; rm(tmp)
rownames(gArrPos) = rownames(yArrSum)

# create graph object
g = graph.adjacency(yArrSum, mode='directed', diag=FALSE, weighted=TRUE)

# add attribs
g$labPrint = names(V(g))
g$labSize = rescale(degree(g), c(.4, .7))
g$vSize = rescale(degree(g), c(10, 16))
g$vShape = rep('circle', length(V(g)))
g$vShape[match(c('Military\n(Nigeria)','Police\n(Nigeria)'), names(V(g)))] = 'square'

# add cols for degree
pal=colorRampPalette(c(brewer.pal(9,'Blues')[1],brewer.pal(9,'Blues')[9]))(length(seq(0,50,5)))
degBkts = as.numeric(cut(degree(g), seq(0,50,5) ))
V(g)$vCol=pal[degBkts]
V(g)$vLabCol=rep('black', length(V(g))) ; V(g)$vLabCol[degBkts>3] = 'white'

# # 
# gArrPos = layout.circle(g) ; 

# plot by t
selT = rep(FALSE, dim(yArr)[3])
lapply(1:length(yList), function(t){
	selT[t] = TRUE
	tmp = getGraphfromMat(
		mat=getMatfromArr(yArr, pds=selT), nodePos=gArrPos,
		colBrksByDegree=seq(0,12,3),
		)
	g = tmp$g ; pos = tmp$nodePos ; rm(tmp)
	vCol = ifelse(names(V(g)) %in% govActors, 'gray30', 'gray95')
	vLabCol = ifelse(names(V(g)) %in% govActors, 'white', 'gray30')	
	# vCol = 'gray60' ; vLabCol = 'gray60'
	fName = paste0(pathGraphics, 'old/civVicConfNet/nigeriaT_', names(yList)[t], '.png')
	png(filename=fName, width=500,height=500)
	plotGraph(g, pos, 
		eCurve=FALSE, arrowSize=.5,		
		vShape='circle', 
		vLabCex=.0001,
		vertex.size=g$vSize, 
		# vertex.label.font=1, vertex.label.family="Helvetica", 
		vLabCol=vLabCol, 
		vertex.label=NA,
		vFrameCol='black', vCol=vCol
		# rescale=TRUE,
		# ylim=c(min(gArrPos[,2]), max(gArrPos[,2])),
		# xlim=c(min(gArrPos[,1]), max(gArrPos[,1]))
		)
	# title(names(yList)[t], family='Helvetica', adj=1, line=-2, cex.main=1)
	dev.off()
})

# plot by t buckets 
yrBkts = list(2001:2004, 2005:2008, 2009:2012, 2013:2016)
lapply(1:length(yrBkts), function(i){
	#
	y = getMatfromArr(yArr, pds=char(yrBkts[[i]]))
	# y = y[-which(rownames(y)=="Ilajes\nMilitia"),-which(rownames(y)=="Ilajes\nMilitia")]
	print(mean(y, na.rm=TRUE))
	tmp = getGraphfromMat(
		mat=y, nodePos=gArrPos,
		colBrksByDegree=seq(0,12,3),
		)
	g = tmp$g ; pos = tmp$nodePos ; rm(tmp)
	vCol = ifelse(names(V(g)) %in% govActors, 'gray30', 'gray95')
	vLabCol = ifelse(names(V(g)) %in% govActors, 'white', 'gray30')	
	# vCol = 'gray60' ; vLabCol = 'gray60'
	fName = paste0(pathGraphics, 'old/civVicConfNet/nigeriaYrBkt_', i, '.png')
	png(filename=fName, width=500,height=500)
	plotGraph(g, pos, 
		eCurve=FALSE, arrowSize=.5,		
		vShape='circle', 
		vLabCex=.0001,
		vertex.size=g$vSize, 
		# vertex.label.font=1, vertex.label.family="Helvetica", 
		vLabCol=vLabCol, 
		vertex.label=NA,
		vFrameCol='black', vCol=vCol
		# rescale=TRUE,
		# ylim=c(min(gArrPos[,2]), max(gArrPos[,2])),
		# xlim=c(min(gArrPos[,1]), max(gArrPos[,1]))
		)
	# title(names(yList)[t], family='Helvetica', adj=1, line=-2, cex.main=1)
	dev.off()
})
################	