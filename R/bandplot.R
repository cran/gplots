# $Id: bandplot.R 1272 2008-05-20 01:07:28Z warnes $

bandplot  <-  function(x,y,
                       ...,
                       add=FALSE,
                       sd=c(-2:2),
                       sd.col=c("magenta","blue","red",
                                 "blue","magenta"),
                       sd.lwd=c(2,2,3,2,2),
                       sd.lty=c(2,1,1,1,2),
                       method="frac", width=1/5,
                       n=50
                       )
  {

    if(length(sd.col)<length(sd)) sd <-rep(sd.col,length=length(sd))
    if(length(sd.lwd)<length(sd)) sd <-rep(sd.lwd,length=length(sd))
    if(length(sd.lty)<length(sd)) sd <-rep(sd.lty,length=length(sd))    
    if(!add)
      {
        m <- match.call(expand.dots = TRUE)
        m$width  <- m$add  <- m$sd  <- m$sd.col  <- NULL
        m$method <- m$n <- m$sd.lty <- m$sd.lwd  <- NULL
        m[[1]] <- as.name("plot")
        mf <- eval(m, parent.frame())
      }

    x  <- as.numeric(x)
    y  <- as.numeric(y)

    CL <- function(x,sd)
      if(length(x)<2)
        return( NA )
      else
        mean(x)+sd*sqrt(var(x))

    ord <- order(x)
    myx <- x[ord]
    myy <- y[ord]
    sdVec <- runsd(myy, k=floor(length(x)*width))
    meanVec <- runmean(myy, k=floor(length(x)*width) )

    stdevs <- list()
    for( i in 1:length(sd) )
      {
          lines(myx,
                meanVec + sdVec * sd[i],
                col=sd.col[i],
                lwd=sd.lwd[i],
                lty=sd.lty[i]
                )

      }
    
  }
