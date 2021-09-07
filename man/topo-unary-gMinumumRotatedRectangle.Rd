\name{gMinumumRotatedRectangle}
\alias{gMinumumRotatedRectangle}

\title{Centroid of Geometry}
\description{Returns the minimum rotated rectangular POLYGON which encloses the input geometry.}
\usage{
gMinumumRotatedRectangle(spgeom, byid=FALSE, id = NULL)
}

\arguments{
  \item{spgeom}{sp object as defined in package sp}
  \item{byid}{Logical determining if the function should be applied across subgeometries (TRUE) or the entire object (FALSE)}
  \item{id}{Character vector defining id labels for the resulting geometries, if unspecified returned geometries will be labeled based on their parent geometries' labels.}
}

\details{Returns the minimum rotated rectangular POLYGON which encloses the input geometry. The rectangle has width equal to the minimum diameter, and a longer length. If the convex hill of the input is degenerate (a line or point) a LINESTRING or POINT is returned. The minimum rotated rectangle can be used as an extremely generalized representation for the given geometry.}

\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gBoundary}}
\code{\link{gConvexHull}}
\code{\link{gEnvelope}}
\code{\link{gPointOnSurface}}
}

\examples{

if (version_GEOS0() >= "3.9.0") {
x = readWKT(paste("GEOMETRYCOLLECTION(POLYGON((0 0,10 1,10 11,0 10,0 0)),",
 "POLYGON((15 0,25 12,35 3,15 0)))"))

# Minimum rotated rectangles of both the square and circle independently
c1 = gMinumumRotatedRectangle(x,byid=TRUE) 
# Minimum rotated rectangle of square and circle together
c2 = gMinumumRotatedRectangle(x)

opar <- par(mfrow=c(2,1))
plot(c1, border='red', lwd=2, lty=2)
plot(x,add=TRUE)
plot(c2, border='blue', lwd=2, lty=3)
plot(x,add=TRUE)
par(opar)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

