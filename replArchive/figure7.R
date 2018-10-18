################
# workspace
source('setup.R')
source('ggCirc.R')
source('actorInfo.R')
################

################
# load data
load('nigeriaMatList_acled_v7.rda') # loads yList object
yrs = char(2000:2016) ; yList = yList[yrs]

load('ameResults.rda') # load AME mod results
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
ySimp = yArrSumm
uSimp = ameFits$base$U
vSimp = ameFits$base$V

uvCols = brewer.pal(11, 'RdBu')[c(11-2, 3)]
facet_labeller = function(string){ TeX(string) }
circPlot=ggCirc(
	Y=ySimp, U=uSimp, V=vSimp, vscale=1, force=5,
	uLabel='Groups with Common Sending Patterns ($u_{i}$)',
	vLabel='Groups with Common Receiving Patterns ($v_{j}$)',
	removeIsolates=FALSE, showActLinks=FALSE) +
	scale_color_manual(values=uvCols) +
	facet_wrap(~eff, 
		ncol=2, 
		labeller=as_labeller(facet_labeller, default = label_parsed) ) +
	theme(
		strip.text.x = element_text(size = 16, color='white',
			angle=0, hjust=.2),
		strip.background = element_rect(fill = "#525252", color='#525252')
		)

ggsave(circPlot, 
	file='figure7.pdf',
	width=12, height=6)
################