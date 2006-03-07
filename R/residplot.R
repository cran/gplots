# $Id: residplot.R 625 2005-06-09 14:20:30Z nj7w $

residplot  <-  function(model, formula, ...)
  {
    data  <- expand.model.frame( model, formula, na.expand=TRUE)

    newform  <- eval(parse( text=paste("as.call(", "resid(model) ~",
                        formula[-1],")" )))

    plot( newform, data=data, ylab="Residuals")
    lines(lowess( newform, data=data ), col="red")
    bandplot(newform,data=data)
  }

