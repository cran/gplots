\name{rich.colors}
\alias{rich.colors}
\title{
  Rich color palettes
}
\description{
  Create a vector of \code{n} colors that are perceptually equidistant
  and in an order that is easy to interpret.
}
\usage{
rich.colors(n, palette="temperature", rgb.matrix=FALSE,
            plot.colors=FALSE)
}
\arguments{
  \item{n}{number of colors to generate.}
  \item{palette}{palette to use: \code{"temperature"} contains
    blue-green-yellow-red, and \code{"blues"} contains
    black-blue-white.}
  \item{rgb.matrix}{if \code{TRUE} then a matrix of RGB values is
    included as an attribute.}
  \item{plot.colors}{if \code{TRUE} then a descriptive color diagram is
    plotted on the current device.}
}
\value{
  A character vector of color codes.
}
\author{Arni Magnusson \email{arnima@u.washington.edu}}
\seealso{
  \code{\link{rgb}},
  \code{\link{rainbow}},
  \code{\link{heat.colors}}.
}
\examples{
m <- matrix(1:120+rnorm(120), nrow=15, ncol=8)
opar <- par(bg="gray", mfrow=c(1,2))
matplot(m, type="l", lty=1, lwd=3, col=rich.colors(8))
matplot(m, type="l", lty=1, lwd=3, col=rich.colors(8,"blues"))
par(opar)

barplot(rep(1,100), col=rich.colors(100), space=0, border=0, axes=FALSE)
barplot(rep(1,20), col=rich.colors(40)[11:30]) # choose subset

rich.colors(100, plot=TRUE, rgb=TRUE)  # describe rgb recipe

par(mfrow=c(2,2))
barplot(m, col=heat.colors(15), main="\nheat.colors")
barplot(m, col=1:15, main="\ndefault palette")
barplot(m, col=rich.colors(15), main="\nrich.colors")
barplot(m, col=rainbow(15), main="\nrainbow")
par(opar)
}
% Graphics
\keyword{color}
