#' Plot results of goodness of fit tests
#' 
#' @param GOF matrix of GOF values from ame_repL
#' @param symmetric logical: Is the sociomatrix symmetric by design?
#' @param rvar logical: fit row random effects (asymmetric case)?
#' @param cvar logical: fit column random effects (asymmetric case)? 
#' @param varKey df with two columns, dirty and clean which specify 
#' old and new varnames
#' @return Plot for goodness of fit statistics
#' @author Shahryar Minhas
#' @export
gofPlot2 <- function(GOF, symmetric,varKey=NULL){

    # libs
    suppressMessages(library(ggplot2))
    suppressMessages(library(plyr))
    suppressMessages(library(reshape2))

    # org data
    actGOF <- GOF[1,]
    meltGOF <- reshape2::melt(GOF[-1,])
    ggMu <- plyr::ddply(meltGOF, .(Var2), summarise, mu=mean(value))
    ggMu$actual <- actGOF[match(ggMu$Var2,names(actGOF))]
    ggDens <- plyr::ddply(meltGOF,.(Var2),.fun = function(x){
      tmp <- density(x$value) ; x1 <- tmp$x; y1 <- tmp$y
      q90 <- x1 >= quantile(x$value,0.05) & x1 <= quantile(x$value,0.95)
      q95 <- x1 >= quantile(x$value,0.025) & x1 <= quantile(x$value,0.975)
      data.frame(x=x1,y=y1,q90=q90,q95=q95) })

    ## code to only extend line segments to top of density dist
    # ggMu$yEnd=Inf
    # for(v in as.character(ggMu$Var2)){
    #   densSlice <- ggDens[which(ggDens$Var2==v),]
    #   diff <- abs(densSlice$x-ggMu$mu[ggMu$Var2==v]) 
    #   ggMu$yEnd[ggMu$Var2==v] <- densSlice$y[which(diff==min(diff))]
    # }

    # ggMu$yEndForAct=Inf
    # for(v in as.character(ggMu$Var2)){
    #   densSlice <- ggDens[which(ggDens$Var2==v),]
    #   diff <- abs(densSlice$x-ggMu$actual[ggMu$Var2==v]) 
    #   ggMu$yEndForAct[ggMu$Var2==v] <- densSlice$y[which(diff==min(diff))]
    # }

    # cleanup based on conditions
    ggData <- list(ggMu=ggMu,ggDens=ggDens,meltGOF=meltGOF)
    ggData <- lapply(ggData,function(x){x$Var2<-as.character(x$Var2);return(x)})
    if(symmetric){
        # remove dyadic dep calculation if symmetric
        ggData<-lapply(ggData,function(x){x=x[which(x$Var2!='dyad.dep'),];return(x)})
        # adjust node variation plot
        ggData<-lapply(ggData,function(x){
            x <- x[which(x$Var2!='sd.colmean'),]
            x$Var2[which(x$Var2=='sd.rowmean')] <- 'sd.nodemean'
            return(x) })
    }
    
    # cleanup
    ggData<-lapply(ggData,function(x){
        if(is.null(varKey)){
            x$Var2<-factor(x$Var2,
                levels=c('sd.rowmean','sd.colmean','sd.nodemean','dyad.dep','triad.dep'))
        }
        if(!is.null(varKey)){
            x$Var2 <- varKey$clean[match(x$Var2, varKey$dirty)]
            x$Var2 <- factor(x$Var2, levels = varKey$clean)
        }
        return(x) })
    ggDens<-ggData$ggDens;ggMu<-ggData$ggMu;meltGOF<-ggData$meltGOF;rm(ggData)

    # plot
    ggGOF <- ggplot2::ggplot(ggDens, aes(x=x)) +
      geom_histogram(data=meltGOF, aes(x=value, y=..density..), 
        fill='grey95', color='black', lwd=1.25) +  
      geom_line(aes(y=y), color='black', lwd=1) +
      geom_ribbon(data=subset(ggDens, q90), aes(ymax=y),ymin=0, alpha=.6, fill='grey20') +
      geom_ribbon(data=subset(ggDens, q95), aes(ymax=y),ymin=0, alpha=.5, fill='grey20') +
      geom_vline(data=ggMu, aes(xintercept=mu),
                 linetype='solid', size=1, color='red') +
      geom_vline(data=ggMu, aes(xintercept=actual),
                 linetype='solid', size=1, color='blue') +  
      # geom_segment(data=ggMu, aes(x=mu,xend=mu,y=0,yend=yEnd), 
      #              linetype='solid', color='red', size=2) +
      # geom_segment(data=ggMu, aes(x=actual,xend=actual,y=0,yend=yEndForAct), 
      #            linetype='solid', size=2, color='blue') +   
      facet_wrap(~ Var2, scales='free') + ylab('') + 
      xlab('Parameter Value\nBlue line denotes actual value and red denotes mean of simulated.\nShaded interval represents 90 and 95 percent credible intervals.') +  
      theme_bw() + 
      theme(
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        axis.text.y=element_blank()
      )

  return( ggGOF )
}
