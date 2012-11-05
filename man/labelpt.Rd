\name{calc.labpt.strings}
\alias{calc.labpt.strings}
\alias{calc.labpt.strings}
\alias{polygonsLabel}

\title{optimally compute label points}
\description{optimally compute label points}
\usage{
calc.labpt.strings(pol, label, gridpoints = 50, cex = 1,...)
polygonsLabel(pol, labels, ...)
}

\arguments{
  \item{pol}{ object of class or deriving from \code{SpatialPolygons}}
  \item{label}{ character; vector of length one with label}
  \item{labels}{ character; vector with labels}
  \item{gridpoints}{ integer; number of grid points to try }
  \item{cex}{numeric; magnification factor for text labels}
  \item{...}{ argument(s) passed on to \code{calc.labpt.strings}, and from
  there to \code{strwidth} and \code{strheight}}
}

\value{ 

This version of the code, which uses Nelder-Mead to zero in on the
'optimal' label position.

But as alluded to in an earlier post, it won't necessarily work 
well for more regular polygons, as it's only optimal in the sense 
that the label position is *a* position with greatest distance to the
polygon boundary. If there are many positions, this might be far from
the visually optimal position. 

In the example below, the 'Hi!' label will always be placed in
centre of the square, but the position of the two other labels
can differ much (rerun the code to see them change). For example,
the 'Narrow' label can be positioned almost at the bottom or
the top of the narrow rectangle (but always horizontally in the
middle), since moving it up or down doesn't change the (shortest)
distance to the polygon boundary.

\code{calc.lbpt.strings} is for use on a SpatialPolygons object
with a single (multi)polygon).

}

\author{ Karl Ove Hufthammer}

\examples{
x=c(0,1.8,1.8,1,1,3,3,2.2,2.2,4,4,6,6,14,14,6,6,4,4,0,0)
y=c(0,0,-2,-2,-10,-10,-2,-2,0,0,1.8,1.8,1,1,3,3,2.2,2.2,4,4,0)
xy=data.frame(x,y)

xy.sp=SpatialPolygons(list(Polygons(list(Polygon(xy)), ID="test")))

labels=c("Hi!", "A very long text string", "N\na\nr\nr\no\nw")
plot(xy.sp, col="khaki")
for( label in labels) {
  xy=calc.labpt.strings(xy.sp, label)
  text(xy[1], xy[2], label)
}
plot(xy.sp, col="khaki")

# example with multiple polygons & text labels
x1=c(0,4,4,0,0)
y1=c(0,0,4,4,0)
x2=c(1,1,3,3,1)
y2=c(-2,-10,-10,-2,-2)
x3=c(6,14,14,6,6)
y3=c(1,1,3,3,1)
xy.sp=SpatialPolygons(list(
	Polygons(list(Polygon(cbind(x1,y1))), ID="test1"), # box
	Polygons(list(Polygon(cbind(x3,y3))), ID="test3"), # wide
	Polygons(list(Polygon(cbind(x2,y2))), ID="test2")  # high
	))
plot(xy.sp, col=bpy.colors(3))
text(polygonsLabel(xy.sp, labels), labels, col = c('white', 'black', 'black'))

}

\keyword{spatial}
