\name{sinkplot}
\alias{sinkplot}
\title{Send textual R output to a graphics device}
\description{
  Divert R's standard text output to a graphics device.
}
\usage{
sinkplot(operation = c("start", "plot", "cancel"), ...)
}
\arguments{
  \item{operation}{See below}
  \item{\dots}{Plot arguments. (Ignored unless \code{operation}="plot").}
}
\details{
  This function allows the printed output of R commands to be captured
  and displayed on a graphics device.

  The capture process is started by calling
  \code{sinkplot("start")}. Now R commands can be executed and all
  printed output (except errors) will be captured.  When the desired
  text has been captured \code{sinkplot("plot")} can be called to
  actually display the output.  \code{sinkplot("cancel")} can be used to
  abort the output capture without plotting.

  The current implementation does not allow \code{sinkplot} to be nested.
}
\value{
  Invisibly returns a character vector containing one element for each
  line of the captured output.
}

\references{Functionality requested by Kevin Wright
   \email{kwright@eskimo.com} in the R-devel newlist posting
   \url{https://www.stat.math.ethz.ch/pipermail/r-devel/2004-January/028483.html}}
\author{ Gregory R. Warnes \email{gregory.r.warnes@pfizer.com}  }
\seealso{ \code{\link[utils]{capture.output}}, \code{\link{textplot}} }
\examples{
\dontrun{
   set.seed(12456)
   x <- factor(sample( LETTERS[1:5], 50, replace=TRUE))
   y <- rnorm(50, mean=as.numeric(x), sd=1)


   par(mfrow=c(1,2))
   boxplot(y~x, col="darkgreen")

   sinkplot()
   anova(lm(y~x))
   sinkplot("plot",col="darkgreen")
}
}
\keyword{hplot}
