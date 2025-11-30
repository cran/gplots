## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 8,
  fig.height = 6,
  fig.align = "center"
)

## ----load-libraries-----------------------------------------------------------
# Load required libraries
library(gplots)  # for heatmap.2
library(RColorBrewer)  # for better color palettes

## -----------------------------------------------------------------------------
data(mtcars)
x  <- as.matrix(mtcars)
rc <- rainbow(nrow(x), start=0, end=.3)
cc <- rainbow(ncol(x), start=0, end=.3)


## -----------------------------------------------------------------------------
heatmap.2(x)                    ## default - dendrogram plotted and reordering done.
heatmap.2(x, dendrogram="none") ##  no dendrogram plotted, but reordering done.
heatmap.2(x, dendrogram="row")  ## row dendrogram plotted and row reordering done.
heatmap.2(x, dendrogram="col")  ## col dendrogram plotted and col reordering done.

heatmap.2(x, keysize=2)         ## default - dendrogram plotted and reordering done.

heatmap.2(x, Rowv=FALSE, dendrogram="both") ## generates a warning!
heatmap.2(x, Rowv=NULL, dendrogram="both")  ## generates a warning!
heatmap.2(x, Colv=FALSE, dendrogram="both") ## generates a warning!


## -----------------------------------------------------------------------------
heatmap.2(x, reorderfun=function(d, w) reorder(d, w, agglo.FUN = mean) )


## ----fig.keep='none'----------------------------------------------------------
full <- heatmap.2(x) # we use it to easily get the dendrograms

## -----------------------------------------------------------------------------
library(dendextend)  # for color_branches
heatmap.2(x, 
        Rowv=color_branches(full$rowDendrogram, k = 3),
        Colv=color_branches(full$colDendrogram, k = 2))
# Look at the vignette for more details:
# https://cran.r-project.org/web/packages/dendextend/vignettes/dendextend.html


## -----------------------------------------------------------------------------
full <- heatmap.2(x)
heatmap.2(x, Colv=full$colDendrogram[[2]], breaks=full$breaks)  # column subset
heatmap.2(x, Rowv=full$rowDendrogram[[1]], breaks=full$breaks)  # row subset
heatmap.2(x, Colv=full$colDendrogram[[2]],
            Rowv=full$rowDendrogram[[1]], breaks=full$breaks)  # both



## -----------------------------------------------------------------------------

heatmap.2(x, srtCol=NULL)
heatmap.2(x, srtCol=0,   adjCol = c(0.5,1) )
heatmap.2(x, srtCol=45,  adjCol = c(1,1)   )
heatmap.2(x, srtCol=135, adjCol = c(1,0)   )
heatmap.2(x, srtCol=180, adjCol = c(0.5,0) )
heatmap.2(x, srtCol=225, adjCol = c(0,0)   ) ## not very useful
heatmap.2(x, srtCol=270, adjCol = c(0,0.5) )
heatmap.2(x, srtCol=315, adjCol = c(0,1)   )
heatmap.2(x, srtCol=360, adjCol = c(0.5,1) )

heatmap.2(x, srtRow=45, adjRow=c(0, 1) )
heatmap.2(x, srtRow=45, adjRow=c(0, 1), srtCol=45, adjCol=c(1,1) )
heatmap.2(x, srtRow=45, adjRow=c(0, 1), srtCol=270, adjCol=c(0,0.5) )


## -----------------------------------------------------------------------------
## not also present)
heatmap.2(x, offsetRow=0, offsetCol=0)
heatmap.2(x, offsetRow=1, offsetCol=1)
heatmap.2(x, offsetRow=2, offsetCol=2)
heatmap.2(x, offsetRow=-1, offsetCol=-1)

heatmap.2(x, srtRow=0, srtCol=90, offsetRow=0, offsetCol=0)
heatmap.2(x, srtRow=0, srtCol=90, offsetRow=1, offsetCol=1)
heatmap.2(x, srtRow=0, srtCol=90, offsetRow=2, offsetCol=2)
heatmap.2(x, srtRow=0, srtCol=90, offsetRow=-1, offsetCol=-1)


## -----------------------------------------------------------------------------
lmat <- rbind( c(5,3,4), c(2,1,4) )
lhei <- c(1.5, 4)
lwid <- c(1.5, 4, 0.75)

myplot <- function() {
           oldpar <- par("mar")
           par(mar=c(5.1, 4.1, 0.5, 0.5))
           plot(mpg ~ hp, data=x)
         }

heatmap.2(x, lmat=lmat, lhei=lhei, lwid=lwid, key=FALSE, extrafun=myplot)


## -----------------------------------------------------------------------------

heatmap.2(x,
         key.title=NA, # no title
         key.xlab=NA,  # no xlab
         key.par=list(mgp=c(1.5, 0.5, 0),
                      mar=c(2.5, 2.5, 1, 0)),
         key.xtickfun=function() {
               breaks <- parent.frame()$breaks
               return(list(
                    at=parent.frame()$scale01(c(breaks[1],
                                                breaks[length(breaks)])),
                    labels=c(as.character(breaks[1]),
                             as.character(breaks[length(breaks)]))
                    ))
         })

heatmap.2(x,
        breaks=256,
        key.title=NA,
        key.xlab=NA,
        key.par=list(mgp=c(1.5, 0.5, 0),
                     mar=c(1, 2.5, 1, 0)),
        key.xtickfun=function() {
             cex <- par("cex")*par("cex.axis")
             side <- 1
             line <- 0
             col <- par("col.axis")
             font <- par("font.axis")
             mtext("low", side=side, at=0, adj=0,
                   line=line, cex=cex, col=col, font=font)
             mtext("high", side=side, at=1, adj=1,
                   line=line, cex=cex, col=col, font=font)
             return(list(labels=FALSE, tick=FALSE))
        })



## -----------------------------------------------------------------------------
##
##
hv <- heatmap.2(x, col=bluered, scale="column", tracecol="#303030")

###
## Look at the return values
###
names(hv)

## Show the mapping of z-score values to color bins
hv$colorTable

## Extract the range associated with white
hv$colorTable[hv$colorTable[,"color"]=="#FFFFFF",]

## Determine the original data values that map to white
whiteBin <- unlist(hv$colorTable[hv$colorTable[,"color"]=="#FFFFFF",1:2])
rbind(whiteBin[1] * hv$colSDs + hv$colMeans,
     whiteBin[2] * hv$colSDs + hv$colMeans )
##
## A more decorative heatmap, with z-score scaling along columns
##
hv <- heatmap.2(x, col=cm.colors(255), scale="column",
       RowSideColors=rc, ColSideColors=cc, margin=c(5, 10),
       xlab="specification variables", ylab= "Car Models",
       main="heatmap(<Mtcars data>, ..., scale=\"column\")",
       tracecol="green", density="density")
## Note that the breakpoints are now symmetric about 0

## Color the labels to match RowSideColors and ColSideColors
hv <- heatmap.2(x, col=cm.colors(255), scale="column",
       RowSideColors=rc, ColSideColors=cc, margin=c(5, 10),
       xlab="specification variables", ylab= "Car Models",
       main="heatmap(<Mtcars data>, ..., scale=\"column\")",
       tracecol="green", density="density", colRow=rc, colCol=cc,
       srtCol=45, adjCol=c(0.5,1))




## -----------------------------------------------------------------------------
# TODO: want example using the `add.exp' argument!

data(attitude)
round(Ca <- cor(attitude), 2)
symnum(Ca) # simple graphic

# with reorder
heatmap.2(Ca, 		 symm=TRUE, margin=c(6, 6), trace="none" )

# without reorder
heatmap.2(Ca, Rowv=FALSE, symm=TRUE, margin=c(6, 6), trace="none" )

## Place the color key below the image plot
heatmap.2(x, lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ) )

## Place the color key to the top right of the image plot
heatmap.2(x, lmat=rbind( c(0, 3, 4), c(2,1,0 ) ), lwid=c(1.5, 4, 2 ) )


## -----------------------------------------------------------------------------
data(USJudgeRatings)
symnum( cU <- cor(USJudgeRatings) )

hU <- heatmap.2(cU, Rowv=FALSE, symm=TRUE, col=topo.colors(16),
            distfun=function(c) as.dist(1 - c), trace="none")

## The Correlation matrix with same reordering:
hM <- format(round(cU, 2))
hM


## -----------------------------------------------------------------------------

heatmap.2(cU, Rowv=FALSE, symm=TRUE, col=rev(heat.colors(16)),
           distfun=function(c) as.dist(1 - c), trace="none",
           cellnote=hM)


## -----------------------------------------------------------------------------
data(mtcars)
x  <- as.matrix(mtcars)

library(heatmaply)
# just use heatmaply instead of heatmap.2
heatmaply(x)


## ----fig.keep='none'----------------------------------------------------------
full <- heatmap.2(x) # we use it to easily get the dendrograms

## -----------------------------------------------------------------------------
## Color branches of dendrograms by cluster membership using dendextend:
heatmaply(x, 
        Rowv=color_branches(full$rowDendrogram, k = 3),
        Colv=color_branches(full$colDendrogram, k = 2))
# Look at the vignette for more details:
# https://cran.r-project.org/web/packages/heatmaply/vignettes/heatmaply.html


## ----session-info-------------------------------------------------------------
sessionInfo()

