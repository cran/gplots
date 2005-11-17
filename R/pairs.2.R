## put histograms on the diagonal
panel.hist <- function(x, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
  }


panel.offdiag <- function(x, y, ...)
  {
    xIsFactor <- is.factor(x) || length(unique(x)) < 10
    yIsFactor <- is.factor(y) || length(unique(y)) < 10    

    if( xIsFactor && yIsFactor)
      {
        par(new=TRUE)

        balloonplot(table(as.factor(x),as.factor(y)),
                    dotsize=2, dotcolor="cyan", main="" )
      }
    else if (xIsFactor)
      {
        par(new=TRUE)
        boxplot(y, x,
                col="cyan",
                #horizontal=TRUE,
                #add=TRUE,
                xaxt="n",
                yaxt="n",                
                xpd=NA)
      }
    else if (yIsFactor)
      {
        par(new=TRUE)
        boxplot(x, y,
                col="cyan",
                horizontal=TRUE,
                #add=TRUE,
                xaxt="n",
                yaxt="n",
                xpd=NA)
      }
    else
      panel.smooth(x,y,...)

  }

pairs.2 <- function(...)
  {
    pairs(...,
          upper.panel=panel.offdiag,
          lower.panel=panel.offdiag,          
          diag.panel=panel.hist,
          cex = 1.5,
          pch = 24,
          bg="light blue",
          cex.labels = 2,
          font.labels=2)

    
  }

