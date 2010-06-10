\name{smartlegend}
\alias{smartlegend}
\title{Place a legend in a specified logical ("top","bottom", "left",
  "right", etc) location on a plot.}
\description{ 
  This function places a legend in a specified logical ("top","bottom", "left",
  "right", etc) location on a plot.
}
\usage{
smartlegend(x = c("left", "center", "right"),
            y = c("top", "center", "bottom"),
            ..., inset = 0.05)
}
\arguments{
  \item{x}{horizontal location on the plot.  One of "left", "center" or
    "right".}
  \item{y}{vertical location on the plot.  One of "top", "center", or
    "bottom".}
  \item{\dots}{arguments for \code{legend}}
  \item{inset}{inset distance from the margin as a fraction of the plot
    region.}
}
\value{
  Same as \code{legend}
}
\author{Gregory R. Warnes \email{greg@warnes.net} }
\seealso{ \code{\link{legend}} }
\examples{

x <- rnorm(100)
y <- rnorm(100, x^2 )
plot(x,y)
abline( lm(y~x), col="red", lty=1, type="b") # regression fit
lines( lowess(y~x), col="blue", lty=2, type="b") # lowess fit

smartlegend(x="center", y="top",
            c("Regression","Locally Linear Smooth"),
            col=c("red","blue"), lty=1:2)

}
\keyword{hplot}
