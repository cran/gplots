####
## Figure 1
####
library(gplots)
library(datasets)

data(Titanic)

dframe <- as.data.frame(Titanic) # convert to 1 entry per row format

survived <- dframe[dframe$Survived=="No",]
attach(survived)

pdf("Figure1.pdf")
 
balloonplot(x=Class, y=list(Age, Sex), z=Freq,
            sort=TRUE, show.zeros=TRUE, cum.margins=FALSE,
            dotcol="white", show.margins=FALSE,
            main="BalloonPlot : Surviving passengers")

dev.off()

detach(survived)



####
## Figure 2
####
library(gplots)
library(datasets)

data(Titanic)

dframe <- as.data.frame(Titanic) # convert to 1 entry per row format

survived <- dframe[dframe$Survived=="No",]
attach(survived)

pdf("Figure2.pdf")

balloonplot(x=Class,
            y=list(Age, Sex),
            z=Freq,
            dotcol = "green",
            sort=TRUE,
            show.zeros=TRUE,
            cum.margins=FALSE,
            main="BalloonPlot : Surviving passengers"
            )
title(main=list("Area is proportional to number of passengers",
           cex=0.9),
      line=0.5)

dev.off()

detach(survived)


####
## Figure 3
####

attach(dframe)
colors <- ifelse( Survived=="Yes", "green", "magenta")

pdf("Figure3.pdf")

balloonplot(x=Class,
            y=list(Survived,Age,Sex),
            z=Freq,
            dotcol = colors,
            show.zeros=TRUE,
            cum.margins=FALSE,
            main="BalloonPlot : Passenger Class by Survival, Age and Sex"
            )
title(main=list("Area is proportional to number of passengers",
           cex=0.9),
      line=0.5)


dev.off()

detach(dframe)

####
## Figure 4
####

attach(dframe)
colors <- ifelse( Survived=="Yes", "green", "magenta")

pdf("Figure4.pdf")

balloonplot(x=Class,
            y=list(Survived, Age, Sex),
            z=Freq,
            sort=TRUE,
            dotcol=colors,
            show.zeros=TRUE,
            main="BalloonPlot : Passenger Class by Survival, Age and Sex"
            )


title(main=list("Area is proportional to number of passengers",
           cex=0.9),
      line=0.5)

legend(3,0.5,
       legend=c("Not survived","Survived"),
       col=c("magenta","green"),
       pch=20,
       cex=.8,
       pt.cex=0.8,
       text.col=c("magenta", "green"),
       horiz=TRUE,
       xjust=0.5,
       bty="n"
       )


dev.off()

detach(dframe)
