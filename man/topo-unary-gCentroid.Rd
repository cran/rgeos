\name{gCentroid}
\alias{gCentroid}
\alias{RGEOSGetCentroid}

\title{Centroid of Geometry}
\description{Function calculates the centroid of the given geometry.}
\usage{
gCentroid(spgeom, byid=FALSE, id = NULL)
}

\arguments{
  \item{spgeom}{sp object as defined in package sp}
  \item{byid}{Logical determining if the function should be applied across subgeometries (TRUE) or the entire object (FALSE)}
  \item{id}{Character vector defining id labels for the resulting geometries, if unspecified returned geometries will be labeled based on their parent geometries' labels.}
}

\details{Returns a SpatialPoints object of the centroid(s) for spgeom.}

\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gBoundary}}
\code{\link{gConvexHull}}
\code{\link{gEnvelope}}
\code{\link{gPointOnSurface}}
}

\examples{

x = readWKT(paste("GEOMETRYCOLLECTION(POLYGON((0 0,10 0,10 10,0 10,0 0)),",
 "POLYGON((15 0,25 15,35 0,15 0)))"))

# Centroids of both the square and circle independently
c1 = gCentroid(x,byid=TRUE) 
# Centroid of square and circle together
c2 = gCentroid(x)

plot(x)
plot(c1,col='red',add=TRUE)
plot(c2,col='blue',add=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

