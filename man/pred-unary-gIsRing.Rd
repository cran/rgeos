\name{gIsRing}
\alias{gIsRing}
\alias{RGEOSisRing}

\title{Is Geometry a Ring?}
\description{Tests if the given geometry is a ring}
\usage{
gIsRing(spgeom, byid = FALSE) 
}

\arguments{
  \item{spgeom}{sp object as defined in package sp}
  \item{byid}{Logical determining if the function should be applied across subgeometries (TRUE) or the entire object (FALSE)}
}

\value{Returns TRUE if the geometry is a LINEARRING.

Returns TRUE if the geometry is a LINESTRING that is both Simple (\link{gIsSimple}) and Closed (end points intersect), FALSE otherwise.

Returns FALSE if the geometry is a [MULTI]POINT, MULTILINESTRING, or [MULTI]POLYGON.}

\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gIsEmpty}}
\code{\link{gIsSimple}}
\code{\link{gIsValid}}
}

\examples{

l1 = readWKT("LINESTRING(0 0, 1 1, 1 0, 0 1, 0 0)")
l2 = readWKT("LINESTRING(0 0,1 0,1 1,0 1,0 0)")
r1 = readWKT("LINEARRING(0 0, 1 1, 1 0, 0 1, 0 0)")
r2 = readWKT("LINEARRING(0 0,1 0,1 1,0 1,0 0)")
p1 = readWKT("POLYGON((0 0, 1 1, 1 0, 0 1, 0 0))")
p2 = readWKT("POLYGON((0 0,1 0,1 1,0 1,0 0))")

par(mfrow=c(3,2))
plot(l1);title(paste("LINESTRING\nRing:",gIsRing(l1)))
plot(l2);title(paste("LINESTRING\nRing:",gIsRing(l2)))
plot(r1);title(paste("LINEARRING\nRing:",gIsRing(r1)))
plot(r2);title(paste("LINEARRING\nRing:",gIsRing(r2)))
plot(p1);title(paste("POLYGON\nRing:",gIsRing(p1)))
plot(p2);title(paste("POLYGON\nRing:",gIsRing(p2)))
}

\keyword{spatial}

