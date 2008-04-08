# $Id: bandplot.R 1048 2007-02-08 16:56:57Z warnes $

bandplot  <-  function(x,y,
                       ...,
                       add=FALSE,
                       sd=c(-2:2),
                       sd.col=c("lightblue","blue","red",
                                 "blue","lightblue"),
                       sd.lwd=c(1,2,3,2,1),
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

    sdplot <- function(S, COL, LWD, LTY)
                  {
                    where <- wapply(x,y,CL,sd=S,width=width,method=method,n=n)
                    lines(where,col=COL,lwd=LWD,lty=LTY,...)
                    where
                  }

    stdevs <- list()
    for( i in 1:length(sd) )
      stdevs[[as.character(sd[i])]] <- sdplot(
                                              sd[i],
                                              COL=sd.col[i],
                                              LWD=sd.lwd[i],
                                              LTY=sd.lty[i]
                                              )

    invisible( stdevs )
  }
