# $Id: sinkplot.R 1557 2012-06-08 17:56:37Z warnes $

sinkplot <- function(operation=c("start","plot","cancel"),...)
  {
    operation <- match.arg(operation)

    if( operation=="start" )
      {
        if (exists(".sinkplot.conn", envir=globalenv()) &&
            get(".sinkplot.conn", envir=globalenv()) )
          stop("sinkplot already in force")


        .sinkplot.conn <- textConnection(".sinkplot.data", "w", local=FALSE)
        assign(x=".sinkplot.conn", value=.sinkplot.conn, envir=globalenv())

        on.exit(sink())
        sink(.sinkplot.conn)
        on.exit()
      }
    else
      {
        if (!exists(".sinkplot.conn", envir=globalenv()) || !.sinkplot.conn )
          stop("No sinkplot currently in force")

        sink()

        data <- get(".sinkplot.data", envir=globalenv())

        if( operation=="plot" )
            textplot( paste( data, collapse="\n"), ... )

        close(get(".sinkplot.conn", envir=globalenv()))

        if(exists(".sinkplot.data", envir=globalenv()))
          rm(".sinkplot.data", pos=globalenv())

        if(exists(".sinkplot.conn", envir=globalenv()))
          rm(".sinkplot.conn", pos=globalenv())

        invisible(data)
      }
  }
