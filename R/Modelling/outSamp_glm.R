y = melt(yList)
xd = melt(xDyadL) ; xd = cbind(xd[xd$Var3=='govActor',], postBoko=xd$value[xd$Var3=='postBoko'])
names(xd)[4]='govActor'
xr = melt(xRowL)
xc = melt(xColL)
glmData = cbind(y,xd[,c('govActor','postBoko')])
glmData$riotsAgainst = xr$value[match(paste0(glmData$Var1,glmData$L1),paste0(xr$Var1,xr$L1))]
glmData$vioCivEvents = xc$value[match(paste0(glmData$Var2,glmData$L1),paste0(xc$Var1,xc$L1))]
glmMod = glm(value ~ govActor + postBoko + riotsAgainst + vioCivEvents, data=glmData, family='binomial')

library(amen)
y = listToArray(sort(unique(unlist(lapply(yList,rownames)))), yList, NULL, NULL, NULL)$Y
ysumm = apply(y, c(1,2), sum, na.rm=TRUE)