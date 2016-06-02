#### Data prep
# Cast directed dyadic variable into array
castArray = function(dyadData, var, monadic=FALSE, row=FALSE){
	library(reshape2)
	arr = acast(dyadData, i ~ j ~ t, value.var=var)
	arr[is.na(arr)] = 0	
	if(monadic){
		if(row){
			for(t in 1:dim(arr)[3]){
				diag(arr[,,t]) = apply(arr[,,t], 1, function(x){ setdiff(unique(x),0) })
			}
		} else {
			for(t in 1:dim(arr)[3]){
				diag(arr[,,t]) = apply(arr[,,t], 2, function(x){ setdiff(unique(x),0) })
			}
		}
	}
	return(arr)
}

# Creates relational covariate matrix for single variable array
createRelCovar = function(arr, var, incMain=TRUE, incRecip=TRUE, incTrans=TRUE){
	if(incMain){ main = arr }
	if(incRecip){ recip = aperm(arr, c(2,1,3)) }
	if(incTrans){
		trans = arr
		for(mnth in 1:dim(arr)[3]){
			XS = ( trans[,,mnth] + t(trans[,,mnth]) )/2
			trans[,,mnth] = XS %*% XS }
	} 
	# Create new array with relational covariates
	# Additional dimensions
	relDim = incMain + incRecip + incTrans
	# Empty array
	X = array( dim=append(dim(arr), relDim, after=2) )
	# Bring in labels
	dimnames(X)[[1]] = dimnames(X)[[2]] = dimnames(arr)[[1]]
	varLabs = NULL
	if(incMain){ varLabs = append(varLabs, var) }
	if(incRecip){ varLabs = append(varLabs, paste0(var, '_ji')) }
	if(incTrans){ varLabs = append(varLabs, paste0(var, '_ijk')) }
	dimnames(X)[[3]] = varLabs
	dimnames(X)[[4]] = dimnames(arr)[[3]]
	# Organizing entry of elements in array
	logic = c(incMain, incRecip, incTrans) %>% cbind()
	rownames(logic) = c('ij', 'ji', 'ijk')
	logic = logic[logic[,1]==1,,drop=FALSE]
	logic = logic %>% cbind(., cumsum(.[,1]))
	# Insert into empty array
	if(incMain){ X[,,logic['ij',2],] = main }
	if(incRecip){ X[,,logic['ji',2],] = recip }
	if(incTrans){ X[,,logic['ijk',2],] = trans }
	return(X)
}

# Prep data for mltr
prepMLTR = function(
	dyadData=dirDyad, var, 
	incMain=TRUE, incRecip=TRUE, incTrans=TRUE,
	lag = TRUE, tDim=NULL
	){
	arr = castArray(dyadData, var)
	X = createRelCovar(arr, var, incMain, incRecip, incTrans)
	if(lag){ X = X[,,,-tDim,drop=FALSE] }
	return(X)
}


#### Posterior analysis
# array of data, assumes four dimensional array
# vars/times used for which we want summary stats
# infoMat contains information on directed/undirected var
getScenVals = function(
	data=X, vars, 
	time, infoMat){

	# Calculate values from array
	vals = vector("list", length(vars)) ; names(vals) = vars
	for(v in vars){
		for(t in time){
			x = data[,,v,t]
			diag(x) = NA
			if(infoMat[v,'directed']){ x[lower.tri(x)] = NA }
			data[,,v,t] = x }

		# Summary stats
		library(reshape2)
		voiMelt = melt(data[,,v,]) %>% na.omit() # double loop speed is fine, melt is what slows things down
		vals[[v]] = list( 
			mean(voiMelt[,'value']),
			quantile(voiMelt[,'value'], probs=seq(0, 1, .05) )
			)
	}

	return(vals)
}

# Assembles scenario array
# varToVary is a single character item
# valsForScen is a list produced by getScenVals function with numeric values for variables to be used in scenario
# dim12names are the names for the first two dimensions of the array, typically units (e.g., countries)
# dim3names for the third dimension, typically IVs
getScenArray = function(varToVary, valsForScen, dim12names, dim3names){
	# Dimensions of scen matrix
	dimScen = c(length(dim12names), length(dim12names), length(dim3names))
	# Pull out scenario value numbers
	oVals = lapply(valsForScen[setdiff(names(valsForScen), varToVary)], function(x) x[1]) %>% unlist()
	allVals = valsForScen[[varToVary]][[2]] %>% 
		unique() %>% 
		lapply(., function(v){
		pos = which(varToVary == dim3names)
		upd = append(oVals, v, after=(pos-1)) 
		names(upd)[pos] = varToVary
		return(upd) 
		})

	# Empty array for scenario to fill in
	scenArray = array( 
		dim=c(dimScen, length(allVals)), 
		dimnames=list( dim12names, dim12names, dim3names, NULL ) 
		)

	# Fill in scenario array
	for(ii in 1:dim(scenArray)[4] ){ 
		tmp = array(rep( allVals[[ii]], each=length(dim12names)^2 ), dim=dimScen)
		scenArray[,,,ii] = tmp
	}

	# Make diag zero
	for(ii in 1:dim(scenArray)[3] ){
		for(jj in 1:dim(scenArray)[4] ){
			diag(scenArray[,,ii,jj]) = 0 } }

	return(scenArray)
}