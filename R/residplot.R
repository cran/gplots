# $Id: residplot.R,v 1.4 2004/09/03 17:27:45 warneg Exp $

residplot  <-  function(model, formula, ...)
  {
    data  <- expand.model.frame( model, formula, na.expand=TRUE)

    newform  <- eval(parse( text=paste("as.call(", "resid(model) ~",
                        formula[-1],")" )))

    plot( newform, data=data, ylab="Residuals")
    lines(lowess( newform, data=data ), col="red")
    bandplot(newform,data=data)
  }

