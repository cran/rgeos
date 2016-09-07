\name{gEnvelope}
\alias{gEnvelope}
\alias{RGEOSEnvelope}

\title{Envelope of Geometry}
\description{Function calculates the rectangular bounding box for the given geometry}
\usage{
gEnvelope(spgeom, byid=FALSE, id = NULL)
}

\arguments{
  \item{spgeom}{sp object as defined in package sp}
  \item{byid}{Logical determining if the function should be applied across subgeometries (TRUE) or the entire object (FALSE)}
  \item{id}{Character vector defining id labels for the resulting geometries, if unspecified returned geometries will be labeled based on their parent geometries' labels.}
}

\details{Returns the rectangular bounding box as a SpatialPolygons object. If spgeom is a degenerate case (horizontal/vertical line, single point) then the function may return an object with lower dimension (SpatialLines or SpatialPoints) or an invalid polygon.}

\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gBoundary}}
\code{\link{gCentroid}}
\code{\link{gConvexHull}}
\code{\link{gPointOnSurface}}
}

\examples{
x = readWKT(paste("POLYGON((0 40,10 50,0 60,40 60,40 100,50 90,60 100,60",
 "60,100 60,90 50,100 40,60 40,60 0,50 10,40 0,40 40,0 40))"))
env = gEnvelope(x)

plot(x,col='blue',border='blue')
plot(env,add=TRUE)

#Degenerate Cases
gEnvelope(readWKT("POINT(1 1)")) #returns SpatialPoints
gEnvelope(readWKT("LINESTRING(1 1,1 2)")) #invalid polygon
gEnvelope(readWKT("LINESTRING(1 1,2 1)")) #invalid polygon

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

