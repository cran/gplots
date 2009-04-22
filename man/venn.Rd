\name{venn}
\alias{venn}
\title{Plot a Venn diagram}
\description{
Plot Venn diagrams for up to 5 sets
}
\usage{
venn(data, universe=NA, small=0.7, showSetLogicLabel=FALSE,
     simplify=FALSE, show.plot=TRUE)
}
\arguments{
  \item{data}{data to be plotted (see below)}
  \item{universe}{??}
  \item{small}{Character size of group labels}
  \item{showSetLogicLabel}{Logical flag indicating whether the internal
    group label should be displayed}
  \item{simplify}{Logical flag indicating whether unobserved group
    should be omitted.}
  \item{show.plot}{Logical flag indicating whether the plot should be
    displayed.  If false, simply returns the group count matrix.}
}
\details{
  \code{data} should be either a named list of vectors containing
  indexes of group members (1, 2, 3,..) , or a data frame containing indicator
  variables (TRUE, FALSE, TRUE, ..) for group membership.  Group names
  will be taken from the component vector or column names.  
}
\value{
  A matrix of all possible sets of groups, and the observed numer of
  items beloinging to each set of groups is returned invisibly.
  The fist column contains observed counts, subsequent columns contain
  0-1 indicators of group membership.
}
\author{Steffen Moeller \email{steffen\_moeller@gmx.de},
  with cleanup and packaging by Gregory R. Warnes
  \email{greg@random-technologies-llc.com}.}
\examples{

##
## Example using a list of item indexes  belonging to the
## specified group.
##
A<- 1:20
B<- 1:20
C<- 2:20
D<- 3:21
input<-list(A,B,C,D)
input

venn(input)

##
## Example using a data frame of indicator columns
##
A<- as.logical(rbinom(100, 1, 0.2))
B<- as.logical(rbinom(100, 1, 0.7))
C<- as.logical(rbinom(100, 1, 0.2))
D<- as.logical(rbinom(100, 1, 0.1))
input<-data.frame(A,B,C,D)
venn(input)


## Omit un-observed groupings
tmp <- venn(input, simplify=TRUE)

## show details
tmp

## Show internal binary group labels
venn(input, showSetLogicLabel=TRUE)

## Specify universe
venn(input, universe=NULL, showSetLogicLabel=TRUE)
}
\keyword{hplot}
