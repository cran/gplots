% $Id: boxplot.n.Rd 977 2006-08-02 22:21:49Z warnes $
%
% $Log$
% Revision 1.5  2005/12/01 16:46:52  nj7w
% Updated Greg's email address
%
% Revision 1.4  2005/06/09 14:20:28  nj7w
% Updating the version number, and various help files to synchronize splitting of gregmisc bundle in 4 individual components.
%
% Revision 1.1.1.1  2005/05/25 22:15:30  nj7w
% Initial submission as an individual package
%
% Revision 1.3  2002/04/09 00:51:31  warneg
%
% Checkin for version 0.5.3
%
% Revision 1.2  2001/08/25 05:46:21  warneg
% Added CVS header.
%
% Revision 1.1  2001/08/25 05:45:10  warneg
% Initial Checkin
%
\name{boxplot.n}
\alias{boxplot.n}
\title{Produce a Boxplot Annotated with the Number of Observations}
\description{
  This funcntion uses \code{boxplot} to produce a boxplot which is then 
  annotated with the number of observations in each group.
}
\usage{
boxplot.n(..., top=FALSE, shrink=1, textcolor=NULL)
}
\arguments{
  \item{\dots}{ parameters passed to \code{boxplot}. }
  \item{top}{ logical indicating whether the number of observations
    should be added to the top or the bottom of the plotting
    region. Defaults to \code{FALSE}. }
  \item{shrink}{ value to shrink character size (cex) when annotating.}
  \item{textcolor}{ text color. }
}
\author{ Gregory R. Warnes \email{warnes@bst.rochester.edu}}
\seealso{ \code{\link{boxplot}}, \code{\link{text}}}

\examples{
data(state)

# n's at bottom
boxplot.n( state.area ~ state.region)

# n's at top
boxplot.n( state.area ~ state.region, top=TRUE)

# small red text
boxplot.n( state.area ~ state.region, shrink=0.8, textcolor="red") 
}

\keyword{ hplot }
