\name{balloonplot}
\alias{balloonplot}
\alias{balloonplot.default}
\alias{balloonplot.table}
\title{Plot a graphical matrix where each cell contains a dot whose size
  reflects the relative magnitude of the corresponding component.}
\description{
  Plot a graphical matrix where each cell contains a dot whose size
  reflects the relative magnitude of the corresponding component.
}
\usage{
balloonplot(x, ...)
\method{balloonplot}{table}(x, xlab, ylab, zlab, ...)
\method{balloonplot}{default}(x, y, z,
    xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)), 
    zlab = deparse(substitute(z)),
    dotsize = 2/max(strwidth(19), strheight(19)),
    dotchar = 19, dotcolor = "skyblue",
    main, label = TRUE, label.digits = 2,
    scale.method = c("volume", "diameter"),
    colsrt = par("srt"),
    rowsrt = par("srt"),
    colmar = 1,
    rowmar = 1,
     ...) 
}
\arguments{
  \item{x}{Either a table object, or a vector of values for the first
    (x) dimension of the plotted matrix.}
  \item{y}{Vector of values for the second (y) dimension of the plotted
    matrix.}
  \item{z}{Vector of values for the size of the dots in the plotted matrix.}
  \item{xlab}{Text label for the x dimension. This will be displayed on the
    x axis and in the plot title.}
  \item{ylab}{Text label for the y dimension. This will be displayed on the
    y axis and in the plot title.}
  \item{zlab}{Text label for the dot size. This will be included in the
    plot title.}
  \item{dotsize}{Maximum dot size.  You may need to adjust this value
    for different plot devices and layouts.}
  \item{dotchar}{Plotting symbol or character used for dots.  See the help
    page for the points function for symbol codes.}
  \item{dotcolor}{Scalar or vector specifying the color(s) of the dots in
    the plot.}
  \item{main}{Plot title text.}
  \item{label}{Boolean flag indicating whether the actual value of the
    elements should be shown on the plot.}
  \item{label.digits}{Number of digits used in formatting value labels.}
  \item{scale.method}{Method of scaling the sizes of the dot, either
    "volume" or "diameter". See below.}
  \item{rowsrt, colsrt}{Angle of rotation for row and column labels.}
  \item{rowmar, colmar}{Space allocated for row and column labels. Each
    unit is the width/height of one cell in the table.}
  \item{\dots}{ Additional arguments passed to \code{balloonplot.default}
    or \code{plot}, as appropriate.}
}
\details{
  This function plots a visual matrix.  In each \code{x},\code{y} cell a
  dot is plotted which reflects the relative size of the corresponding
  value of \code{z}.  When \code{scale.method="volume"} the volume of
  the dot is proportional to the relative size of \code{z}.  When
  \code{scale.method="diameter"}, the diameter of the dot is proportional to
  the the relative size of \code{z}.  The "volume" method is default
  because the "diameter" method visually exaggerates differences.
}
\value{
  Nothing of interest.
}
\note{
  \code{z} is expected to be non-negative.  The function will still
  operate correctly if there are negative values of \code{z}, but the
  corresponding dots will have 0 size and a warning will be generated.
}

\references{Function inspired by question posed on R-help by Ramon
  Alonso-Allende \email{allende@cnb.uam.es}.}

\author{ Gregory R. Warnes \email{Gregory\_R\_Warnes@groton.pfizer.com}}

\seealso{ \code{\link{plot.table}} }

\examples{
\testonly{
set.seed(12425421)
}

# Create an Example Data Frame Containing Car x Color data
carnames <- c("bmw","renault","mercedes","seat")
carcolors <- c("red","white","silver","green")
datavals <- round(rnorm(16, mean=100, sd=60),1)
data <- data.frame(Car=rep(carnames,4),
                   Color=rep(carcolors, c(4,4,4,4) ),
                   Value=datavals )
# show the data
data

# generate balloon plot with default scaling
balloonplot( data$Car, data$Color, data$Value)


# show margin label rotation & space expansion, using some long labels
levels(data$Car) <- c("BMW: High End, German","Renault: Medium End, French",
 "Mercedes: High End, German", "Seat: Imaginary, Unknown Producer")

# generate balloon plot with default scaling
balloonplot( data$Car, data$Color, data$Value, colmar=3, colsrt=90)

# Create an example using table
xnames <- sample( letters[1:3], 50, replace=2)
ynames <- sample( 1:5, 50, replace=2)

tab <- table(xnames, ynames)

balloonplot(tab)


}
\keyword{dplot}
\keyword{hplot}
