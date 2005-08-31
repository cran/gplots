# $Id: rich.color.R,v 1.3 2005/06/09 14:20:27 nj7w Exp $

rich.colors <- function(n, palette=c("temperature", "blues"),
                        rgb.matrix=FALSE, plot.colors=FALSE)
{
  if(n <= 0)
    return(character(0))

  palette <- match.arg(palette)
  x <- seq(0, 1, length=n)

  if(palette == "temperature") {
    r <- 1 / (1+exp(20-35*x))
    g <- pmin(pmax(0,-0.8+6*x-5*x^2), 1)
    b <- dnorm(x,0.25,0.15) / max(dnorm(x,0.25,0.15))
  }
  else {
    r <-        0.6*x + 0.4*x^2
    g <-        1.5*x - 0.5*x^2
    b <- 0.36 + 2.4*x - 2.0*x^2
    b[x>0.4] <- 1
  }

  rgb.m <- matrix(c(r,g,b), ncol=3,
                  dimnames=list(as.character(seq(length=n)),
                    c("red","green","blue")))
  rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1],v[2],v[3]))

  if(rgb.matrix)
    attr(rich.vector, "rgb.matrix") <- rgb.m

  if(plot.colors) {
    opar <- par("fig", "plt")
    par(fig=c(0,1,0,0.7), plt=c(0.15,0.9,0.2,0.95))
    plot(NA, xlim=c(-0.01,1.01), ylim=c(-0.01,1.01), xlab="Spectrum",
         ylab="", xaxs="i", yaxs="i", axes=FALSE)
    title(ylab="Value", mgp=c(3.5,0,0))
    matlines(x, rgb.m, col=colnames(rgb.m), lty=1, lwd=3)
    matpoints(x, rgb.m, col=colnames(rgb.m), pch=16)
    axis(1, at=0:1)
    axis(2, at=0:1, las=1)
    par(fig=c(0,1,0.75,0.9), plt=c(0.08,0.97,0,1), new=TRUE)
    midpoints <- barplot(rep(1,n), col=rich.vector, border=FALSE,
                         space=FALSE, axes=FALSE)
    axis(1, at=midpoints, labels=1:n, lty=0, cex.axis=0.6)
    par(opar)
  }
  return(rich.vector)
}
