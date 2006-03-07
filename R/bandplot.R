# $Id: bandplot.R 625 2005-06-09 14:20:30Z nj7w $

bandplot  <-  function(x,y,
                       ...,
                       add=FALSE,
                       sd=c(-2:2),
                       sd.col=c("lightblue","blue","red",
                                 "blue","lightblue"),
                       method="frac", width=1/5,
                       n=50
                       )
  {

    if(length(sd.col)<length(sd)) sd <-rep(sd.col,length=length(sd))

    if(!add)
      {
        m <- match.call(expand.dots = TRUE)
        m$width  <- m$add  <- m$sd  <- m$sd.col  <- NULL
        m$method <- m$n <- NULL
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

    sdplot <- function(S, COL)
                  {
                    where <- wapply(x,y,CL,sd=S,width=width,method=method,n=n)
                    lines(where,col=COL,...)
                    where
                  }

    stdevs <- list()
    for( i in 1:length(sd) )
      stdevs[[as.character(sd[i])]] <- sdplot( sd[i], sd.col[i] )

    invisible( stdevs )
  }
