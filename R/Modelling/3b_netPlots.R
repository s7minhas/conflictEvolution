################
# workspace
if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	source('~/Research/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='cassydorff' | Sys.info()['user']=='cassydorff'){
	source('~/ProjectsGit/conflictEvolution/R/setup.R')  }
if(Sys.info()['user']=='maxgallop'){
	source('~/Documents/conflictEvolution/R/setup.R')  }
source(paste0(fPth, 'ggCirc.R'))
source(paste0(fPth, 'actorInfo.R'))
################

################
# load data
load(paste0(pathData, 'nigeriaMatList_acled_v7.rda')) # loads yList object
yrs = char(2000:2016) ; yList = yList[yrs]

load(paste0(pathResults, 'ameResults.rda')) # load AME mod results
yArr = listToArray(actors=sort(unique(unlist(lapply(yList,rownames)))), 
	Y=yList, Xdyad=NULL, Xrow=NULL, Xcol=NULL)$Y
yArrSumm = apply(yArr, c(1,2), sum, na.rm=TRUE)
diag(yArrSumm) = 0

# adjust actor names
vNameKey = getNameKey(yList)
rownames(yArrSumm) = vNameKey$clean[match(rownames(yArrSumm), vNameKey$dirty)]
colnames(yArrSumm) = vNameKey$clean[match(colnames(yArrSumm), vNameKey$dirty)]
rownames(ameFits$base$U) = vNameKey$clean[match(rownames(ameFits$base$U), vNameKey$dirty)]
rownames(ameFits$base$V) = vNameKey$clean[match(rownames(ameFits$base$V), vNameKey$dirty)]
################

################
uvCols = brewer.pal(11, 'RdBu')[c(11-2, 3)]
circPlot=ggCirc(
	Y=yArrSumm, U=ameFits$base$U, V=ameFits$base$V, vscale=.6, 
	family="Source Sans Pro Light", force=3, 
	lcol='gray85', lsize=.05) +
	scale_color_manual(values=uvCols)
ggsave(circPlot, 
	file=paste0(pathGraphics,'circPlot.pdf'), 
	width=12, height=10, device=cairo_pdf)
################

################
toDrop = grep('(Nigeria)', rownames(yArrSumm))
ySimp = yArrSumm[-toDrop,] ; ySimp = ySimp[,-toDrop] 
uSimp = ameFits$base$U[-toDrop,]
vSimp = ameFits$base$V[-toDrop,]

uvCols = brewer.pal(11, 'RdBu')[c(11-2, 3)]
circPlot=ggCirc(
	Y=ySimp, U=uSimp, V=vSimp, vscale=.7, 
	family="Source Sans Pro Light", 
	force=1, 
	removeIsolates=FALSE, showActLinks=FALSE) +
	scale_color_manual(values=uvCols)
ggsave(circPlot, 
	file=paste0(pathGraphics, 'circPlotSimple.pdf'), 
	width=12, height=10, device=cairo_pdf)
################

################
toDrop = grep('(Nigeria)', rownames(yArrSumm))
ySimp = yArrSumm[-toDrop,] ; ySimp = ySimp[,-toDrop] 
uSimp = ameFits$base$U[-toDrop,]
vSimp = ameFits$base$V[-toDrop,]

uvCols = brewer.pal(11, 'RdBu')[c(11-2, 3)]
facet_labeller = function(string){ TeX(string) }

circPlot=ggCirc(
	Y=ySimp, U=uSimp, V=vSimp, vscale=.7, force=5,
	family="Source Sans Pro Light", 
	uLabel='Groups with Common Sending Patterns ($u_{i}$)',
	vLabel='Groups with Common Receiving Patterns ($v_{j}$)',
	removeIsolates=FALSE, showActLinks=FALSE) +
	scale_color_manual(values=uvCols) +
	facet_wrap(~eff, 
		ncol=2, 
		labeller=as_labeller(facet_labeller, default = label_parsed) ) +
	theme(
		strip.text.x = element_text(size = 16, color='white',
			family="Source Sans Pro Semibold", angle=0, hjust=.2),
		strip.background = element_rect(fill = "#525252", color='#525252')
		)

ggsave(circPlot, 
	file=paste0(pathGraphics, 'circPlotSimplev2.pdf'), 
	width=12, height=6, device=cairo_pdf)
################

################
ySimp = yArrSumm
uSimp = ameFits$base$U
vSimp = ameFits$base$V

uvCols = brewer.pal(11, 'RdBu')[c(11-2, 3)]
facet_labeller = function(string){ TeX(string) }

circPlot=ggCirc(
	Y=ySimp, U=uSimp, V=vSimp, vscale=1, force=5,
	family="Source Sans Pro Light", 
	uLabel='Groups with Common Sending Patterns ($u_{i}$)',
	vLabel='Groups with Common Receiving Patterns ($v_{j}$)',
	removeIsolates=FALSE, showActLinks=FALSE) +
	scale_color_manual(values=uvCols) +
	facet_wrap(~eff, 
		ncol=2, 
		labeller=as_labeller(facet_labeller, default = label_parsed) ) +
	theme(
		strip.text.x = element_text(size = 16, color='white',
			family="Source Sans Pro Semibold", angle=0, hjust=.2),
		strip.background = element_rect(fill = "#525252", color='#525252')
		)

ggsave(circPlot, 
	file=paste0(pathGraphics, 'circPlotSimplev3.pdf'), 
	width=12, height=6, device=cairo_pdf)
################

################
# plot vecs on 2d
uDF = data.frame(ameFits$base$U) ; uDF$name = rownames(uDF) ; uDF$type='Sender Factor Space'
vDF = data.frame(ameFits$base$V) ; vDF$name = rownames(vDF) ; vDF$type='Receiver Factor Space'
uvDF = rbind(uDF, vDF) ; uvDF$type = factor(uvDF$type, levels=unique(uvDF$type))
ggplot(uvDF, aes(x=X1, y=X2, color=type, label=name)) + 
	geom_vline(xintercept = 0, linetype='dashed', color='grey50') + 
	geom_hline(yintercept = 0, linetype='dashed', color='grey50') + 
	scale_color_manual(values=rev(uvCols)) + 
	geom_point() + 
	geom_text_repel() + 
	facet_wrap(~type) + 
	xlab('') + ylab('') + 
	labs(color='') + 
	theme(
		legend.position = 'none',
		axis.ticks=element_blank(),
		axis.text=element_blank(),
		panel.border=element_blank()
		)
################