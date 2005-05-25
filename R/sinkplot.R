# $Id: sinkplot.R,v 1.2 2004/09/03 17:27:45 warneg Exp $

sinkplot <- function(operation=c("start","plot","cancel"),...)
  {
    operation <- match.arg(operation)

    if( operation=="start" )
      {
        if (exists(".sinkplot.conn", env=globalenv()) &&
            get(".sinkplot.conn", env=globalenv()) )
          stop("sinkplot already in force")


        .sinkplot.conn <- textConnection(".sinkplot.data", "w", local=FALSE)
        assign(x=".sinkplot.conn", value=.sinkplot.conn, envir=globalenv())

        on.exit(sink())
        sink(.sinkplot.conn)
        on.exit()
      }
    else
      {
        if (!exists(".sinkplot.conn", env=globalenv()) || !.sinkplot.conn )
          stop("No sinkplot currently in force")

        sink()

        data <- get(".sinkplot.data", env=globalenv())

        if( operation=="plot" )
            textplot( paste( data, collapse="\n"), ... )

        close(get(".sinkplot.conn", env=globalenv()))

        if(exists(".sinkplot.data", env=globalenv()))
          rm(".sinkplot.data", pos=globalenv())

        if(exists(".sinkplot.conn", env=globalenv()))
          rm(".sinkplot.conn", pos=globalenv())

        invisible(data)
      }
  }
