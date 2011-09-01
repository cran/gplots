# $Id: hist2d.R 1501 2011-09-02 18:14:37Z warnes $

hist2d <- function(x,
                   y=NULL,
                   nbins=200,
                   same.scale=FALSE,
                   na.rm=TRUE,
                   show=TRUE,
                   col=c("black", heat.colors(12)),
                   FUN=base::length,
                   ... )
  {
    if(is.null(y))
      {
        if(ncol(x) != 2) stop("If y is ommitted, x must be a 2 column matirx")
        y <- x[,2]
        x <- x[,1]
      }

    if(length(nbins)==1)
      nbins <- rep(nbins,2)

    nas <- is.na(x) | is.na(y)

    if(na.rm)
      {
        x <- x[!nas]
        y <- y[!nas]
      }
    else
      stop("missinig values not permitted if na.rm=FALSE")

    if(same.scale)
      {
        x.cuts <- seq( from=min(x,y), to=max(x,y), length=nbins[1]+1, labels=FALSE)
        y.cuts <- seq( from=min(x,y), to=max(x,y), length=nbins[2]+1, labels=FALSE)
      }
    else
      {
        x.cuts <- seq( from=min(x), to=max(x), length=nbins[1]+1, labels=FALSE)
        y.cuts <- seq( from=min(y), to=max(y), length=nbins[2]+1, labels=FALSE)
      }

    index.x <- cut( x, x.cuts, include.lowest=TRUE)
    index.y <- cut( y, y.cuts, include.lowest=TRUE)

    ## tapply is faster than old for() loop, and allows
    ## use of any user-specified summary function 
    m <- tapply(x,list(index.x,index.y),FUN)

    ## If we're using length, set empty cells to 0 instead of NA
    if(identical(FUN,base::length))
      m[is.na(m)] <- 0
    
    xvals <- x.cuts[1:nbins[1]]
    yvals <- y.cuts[1:nbins[2]]

    if(show)
      image( xvals,yvals, m, col=col,...)

    retval <- list()
    retval$counts <- m
    retval$x=xvals
    retval$y=yvals
    retval$nobs=length(x)
    retval$call <- match.call()
    class(retval) <- "hist2d"
    retval
  }
