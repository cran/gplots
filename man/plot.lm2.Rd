\name{plot.lm2}
\alias{plot.lm2}
\title{
  Plots to assess the goodness of fit for the linear model objects
}
\description{
   Plots to assess the goodness of fit for the linear model objects
 }
 \usage{
        plot.lm2(x, which = 1:5, caption = c("Residuals vs Fitted",
                 "Normal Q-Q plot", "Scale-Location plot",
                 "Cook's distance plot"), panel = panel.smooth,
                 sub.caption = deparse(x$call), main = "",
                 ask = interactive() && nb.fig < length(which)
                 && .Device != "postscript", ..., id.n = 3,
                 labels.id = names(residuals(x)),
                 cex.id = 0.75, band = TRUE, rug = TRUE)
}
\arguments{
  \item{x}{lm object}
  \item{which}{Numerical values between 1 and 5, indicating which plots 
    to be shown. The codes are:
    
    1- fitted vs residuals plot
    2- Normal Q-Q plot
    3- Scale-Location plot
    4- Cook's distance plot
    5- residuals vs each predictor plot    
  }
  \item{caption}{ Caption for each type of plot}
  \item{panel}{ function to draw on the existing plot}
  \item{sub.caption}{ SubCaption for the plots }
  \item{main}{Main title of the plot}
  \item{ask}{whether interactive graphics or postscript }
  \item{\dots}{ parameters passed to \code{plot.lm2}. }
  \item{id.n}{ integer value, less than or equal to residuals of lm object }
  \item{labels.id}{Names of the residuals of the lm object}
  \item{cex.id}{Parameter to control the height of text stringsx}
  \item{band}{logical vector indicating whether bandplot should also be plotted }
  \item{rug}{logical vector indicating whether rug should be added to
    the existing plot }
}
\author{Gregory R. Warnes \email{warnes@bst.rochester.edu} and Nitin
  Jain \email{nitin.jain@pfizer.com}}
\examples{
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
     trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
     group <- gl(2,10,20, labels=c("Ctl","Trt"))
     weight <- c(ctl, trt)
     x <- lm(weight ~ group)
    plot.lm2(x, which=1)
plot.lm2(x, which=1:3)
}
\keyword{hplot}
