% $Id: bandplot.Rd 977 2006-08-02 22:21:49Z warnes $
%
% $Log$
% Revision 1.7  2005/12/01 16:46:52  nj7w
% Updated Greg's email address
%
% Revision 1.6  2005/06/09 14:20:28  nj7w
% Updating the version number, and various help files to synchronize splitting of gregmisc bundle in 4 individual components.
%
% Revision 1.1.1.1  2005/05/25 22:15:30  nj7w
% Initial submission as an individual package
%
% Revision 1.5  2003/11/19 14:52:32  warnes
%
% Remove extranious NULL that confused R CMD check.
%
% Revision 1.4  2002/09/23 13:59:30  warnes
% - Modified all files to include CVS Id and Log tags.
%
%

\name{bandplot}
\alias{bandplot}
\title{Plot x-y Points with Locally Smoothed Mean and Standard Deviation}
\description{
  Plot x-y Points with lines showing the locally smoothed mean and
  standard deviation.
  }
\usage{
  bandplot(x, y, ..., add = FALSE, sd = c(-2:2),
           sd.col = c("lightblue", "blue", "red", "blue", "lightblue"),
           method = "frac", width = 1/5, n=50) 
}
\arguments{
  \item{x}{numeric vector of x locations}
  \item{y}{numeric vector of x locations}
  \item{\dots}{Additional plotting parameters. }
  \item{add}{ Boolean indicating whether the local mean and standard
    deviation lines should be added to an existing plot.  Defaults to
    FALSE.}
  \item{sd}{Vector of multiples of the standard devation that should be
    plotted.  \code{0} gives the mean, \code{-1} gives the mean minus
    one standard deviation, etc.  Defaults to -2:2.}
  \item{sd.col}{Color of each plotted line.}
  \item{method, width, n}{ Parameters controlling the smoothing. See the
    help page for \code{\link{wapply}} for details.}
}
\details{

  \code{bandplot} was created to look for changes in the mean or
  variance of scatter plots, particularly plots of regression residuals.
  
  The local mean and standard deviation are calculated by calling
  'wapply'.  By default, bandplot asks wapply to smooth using intervals
  that include the nearest 1/5 of the data.  See the documentation of
  that function for details on the algorithm.
  
}
\value{
  Invisibly returns a list containing the x,y points plotted for each line.
}
\author{ Gregory R. Warnes \email{warnes@bst.rochester.edu}
}
\seealso{ \code{\link{wapply}}, \code{\link{lowess}}}
\examples{

# fixed mean, changing variance
x <- 1:1000
y <- rnorm(1000, mean=1, sd=1 + x/1000 )
bandplot(x,y)

# fixed varance, changing mean
x <- 1:1000
y <- rnorm(1000, mean=x/1000, sd=1)
bandplot(x,y)

#
# changing mean and variance
#
x <- abs(rnorm(500))
y <- rnorm(500, mean=2*x, sd=2+2*x)

# the changing mean and dispersion are hard to see whith the points alone:
plot(x,y )

# regression picks up the mean trend, but not the change in variance
reg <- lm(y~x)
summary(reg)

# using bandplot on the original data helps to show the mean and
# variance trend
bandplot(x,y)

# using bandplot on the residuals helps to see that regression removes
# the mean trend but leaves the trend in variability
bandplot(predict(reg),resid(reg))

}
\keyword{ dplot }
