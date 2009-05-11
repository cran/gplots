% $Id: hist2d.Rd 1319 2009-05-08 22:01:12Z warnes $
%
% $Log$
% Revision 1.6  2005/09/12 15:44:36  nj7w
% Updated Greg's email
%
% Revision 1.5  2005/06/09 14:20:28  nj7w
% Updating the version number, and various help files to synchronize splitting of gregmisc bundle in 4 individual components.
%
% Revision 1.1.1.1  2005/05/25 22:15:30  nj7w
% Initial submission as an individual package
%
% Revision 1.4  2002/10/01 18:16:47  warnes
%
% - Added examples of using 'contour' and 'filled.contour' for display.
%
% Revision 1.3  2002/09/23 13:59:30  warnes
% - Modified all files to include CVS Id and Log tags.
%
%

\name{hist2d}
\alias{hist2d}
\title{ Compute and Plot a 2-Dimensional Histogram }
\description{
  Compute and plot a 2-dimensional histogram.
}
\usage{
hist2d(x,y=NULL, nbins=200, same.scale=FALSE, na.rm=TRUE, show=TRUE,
       col=c("black", heat.colors(12)), ... )
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{either a vector containing the x coordinates
    or a matrix with 2 columns. }
  \item{y}{a vector contianing the y coordinates, not required if `x'
    is matrix}
  \item{nbins}{number of bins in each dimension. May be a scalar or a
    2 element vector.  Defaults to 200.}
  \item{same.scale}{use a single range for x and y. Defaults to FALSE.}
  \item{na.rm}{ Indicates whether missing values should be
    removed. Defaults to TRUE.}
  \item{show}{ Indicates whether the histogram be displayed using
    \code{image} once it has
    been computed. Defaults to TRUE.}
  \item{col}{ Colors for the histogram. Defaults to "black" for
    bins containing no elements, a set of 16 heat colors for other bins.}
  \item{\dots}{ Parameters passed to the image function. }
}
\details{
  This fucntion creates a 2-dimensional histogram by cutting the x and
  y dimensions into \code{nbins} sections.  A 2-dimensional matrix is
  then constucted which holds the counts of the number of observed (x,y) pairs
  that fall into each bin.  If \code{show=TRUE}, this matrix is then
  then passed to \code{image} for display.

}
\value{
  A list containing 3 elements:
  \item{counts}{Matrix containing the number of points falling into each
    bin}
  \item{x}{lower x limit of each bin}
  \item{y}{lower y limit of each bin}
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Gregory R. Warnes \email{greg@random-technologies-llc.com}}
%\note{ ~~further notes~~ }

\seealso{ \code{\link{image}}, \code{\link{persp}}, \code{\link{hist}} }

\examples{
   # example data, bivariate normal, no correlation
   x <- rnorm(2000, sd=4)
   y <- rnorm(2000, sd=1)

   # separate scales for each axis, this looks circular
   hist2d(x,y)

   # same scale for each axis, this looks oval
   hist2d(x,y, same.scale=TRUE)

   # use different # bins in each dimension
   hist2d(x,y, same.scale=TRUE, nbins=c(100,200) )

   # use the hist2d function to create inputs for a perspective plot ...
   h2d <- hist2d(x,y,show=FALSE, same.scale=TRUE, nbins=c(20,30))
   persp( h2d$x, h2d$y, h2d$counts,
          ticktype="detailed", theta=30, phi=30,
          expand=0.5, shade=0.5, col="cyan", ltheta=-30)

   # for contour (line) plot ...
   contour( h2d$x, h2d$y, h2d$counts, nlevels=4 )

   # for a filled contour plot ...
   filled.contour( h2d$x, h2d$y, h2d$counts, nlevels=4,
                   col=gray((4:0)/4) )
}
\keyword{dplot}
\keyword{hplot}
