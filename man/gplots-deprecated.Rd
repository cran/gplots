\name{gplots-deprecated}
\alias{boxplot.n}
\alias{plot.lm2}
\title{Deprecated functions}
\description{
  These funcntion have been deprecated and will be removed in future
  releases of gplots.
}
\usage{
   boxplot.n(..., top=FALSE, shrink=1, textcolor=NULL)
   plot.lm2(
            x,
            which = 1:5,
            caption = c("Residuals vs Fitted", "Normal Q-Q plot",
              "Scale-Location plot", "Cook's distance plot"),
            panel = panel.smooth,
            sub.caption = deparse(x$call),
            main = "",
            ask,
            ...,
            id.n = 3,
            labels.id = names(residuals(x)),
            cex.id = 0.75,
            band=TRUE,
            rug=TRUE,
            width=1/10,
            max.n=5000
            )
}
\arguments{
  \item{\dots}{see man page for the corresponding replacement function}
  \item{top, shrink, textcolor}{See man page for
    \code{\link{boxplot2}}.}
  \item{x, which, caption, panel, sub.caption, main, ask, id.n,
    labels.id, cex.id, band, rug, width, max.n}{See man page for
    \code{\link{lmplot2}}.}
}
\details{
These functions have been deprecated.  Please refer to the manual page
for the replacement function:
  \itemize{
    \item \code{boxplot.n} has been replaced by \code{\link{boxplot2}} 
    \item \code{plot.lm2} has been replaced by \code{\link{lmplot2}}
  }
}
\author{Gregory R. Warnes \email{greg@warnes.net}}
\seealso{
  \code{\link{boxplot2}},
  \code{\link{lmplot2}},
  \code{\link[base]{Deprecated}}
}
\keyword{misc}
