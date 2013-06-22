\name{gConvexHull}
\alias{gConvexHull}
\alias{RGEOSConvexHull}

\title{Convex Hull of Geometry}
\description{Function produces the Convex Hull of the given geometry, the smallest convex polygon that contains all subgeometries}
\usage{
gConvexHull(spgeom, byid=FALSE, id = NULL)
}

\arguments{
  \item{spgeom}{sp object as defined in package sp}
  \item{byid}{Logical determining if the function should be applied across subgeometries (TRUE) or the entire object (FALSE)}
  \item{id}{Character vector defining id labels for the resulting geometries, if unspecified returned geometries will be labeled based on their parent geometries' labels.}
}

\details{Returns the convex hull as a SpatialPolygons object.}

\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gBoundary}}
\code{\link{gCentroid}}
\code{\link{gEnvelope}}
\code{\link{gPointOnSurface}}
}

\examples{
x = readWKT(paste("POLYGON((0 40,10 50,0 60,40 60,40 100,50 90,60 100,60",
 "60,100 60,90 50,100 40,60 40,60 0,50 10,40 0,40 40,0 40))"))

ch = gConvexHull(x)

plot(x,col='blue',border='blue')
plot(ch,add=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

