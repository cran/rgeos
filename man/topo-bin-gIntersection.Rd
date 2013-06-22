\name{gIntersection}
\alias{gIntersection}
\title{Geometry Intersections}
\description{Function for determining the intersection between the two given geometries}
\usage{
gIntersection(spgeom1, spgeom2, byid=FALSE, id=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spgeom1, spgeom2}{sp objects as defined in package sp}
  \item{byid}{Logical vector determining if the function should be applied across ids (TRUE) or the entire object (FALSE) for spgeom1 and spgeom2}
  \item{id}{Character vector defining id labels for the resulting geometries, if unspecified returned geometries will be labeled based on their parent geometries' labels.}
}

\details{Returns all spatial intersections as sp objects of the appropriate class. If the geometries do not intersect then an empty geometry is returned.}

\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gDifference}}
\code{\link{gSymdifference}}
\code{\link{gUnion}}
}

\examples{
library(maptools)
xx <- readShapeSpatial(system.file("shapes/fylk-val-ll.shp", package="maptools")[1],
 proj4string=CRS("+proj=longlat +datum=WGS84"))
bbxx <- bbox(xx)
wdb_lines <- system.file("share/wdb_borders_c.b", package="maptools")
xxx <- Rgshhs(wdb_lines, xlim=bbxx[1,], ylim=bbxx[2,])$SP
res <-gIntersection(xx, xxx)
plot(xx, axes=TRUE)
plot(xxx, lty=2, add=TRUE)
plot(res, add=TRUE, pch=16,col='red')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}


