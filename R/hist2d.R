# $Id: hist2d.R,v 1.6 2004/09/03 17:27:44 warneg Exp $

if(is.R())
hist2d <- function( x,y=NULL, nbins=200, same.scale=FALSE, na.rm=TRUE, show=TRUE, col=c("black", heat.colors(12)), ... )
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
        x.cuts <- seq( from=min(x,y), to=max(x,y), length=nbins[1]+1)
        y.cuts <- seq( from=min(x,y), to=max(x,y), length=nbins[2]+1)
      }
    else
      {
        x.cuts <- seq( from=min(x), to=max(x), length=nbins[1]+1)
        y.cuts <- seq( from=min(y), to=max(y), length=nbins[2]+1)
      }



    index.x <- cut( x, x.cuts, include.lowest=TRUE)
    index.y <- cut( y, y.cuts, include.lowest=TRUE)

    m <- matrix( 0, nrow=nbins[1], ncol=nbins[2],
                dimnames=list( levels(index.x),
                               levels(index.y) ) )

    for( i in 1:length(index.x) )
      m[ index.x[i], index.y[i] ] <-  m[ index.x[i], index.y[i] ] + 1

    xvals <- x.cuts[1:nbins[1]]
    yvals <- y.cuts[1:nbins[2]]

    if(show)
      image( xvals,yvals, m, col=col,...)

    invisible(list(counts=m,x=xvals,y=yvals))
  }






