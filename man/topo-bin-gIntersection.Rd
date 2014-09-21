\name{gIntersection}
\alias{gIntersection}
\title{Geometry Intersections}
\description{Function for determining the intersection between the two given geometries}
\usage{
gIntersection(spgeom1, spgeom2, byid=FALSE, id=NULL, drop_not_poly, drop_lower_td=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spgeom1, spgeom2}{sp objects as defined in package sp}
  \item{byid}{Logical vector determining if the function should be applied across ids (TRUE) or the entire object (FALSE) for spgeom1 and spgeom2}
  \item{id}{Character vector defining id labels for the resulting geometries, if unspecified returned geometries will be labeled based on their parent geometries' labels.}
  \item{drop_not_poly}{deprecated argument, use drop_lower_td}
  \item{drop_lower_td}{default FALSE; if TRUE, objects will be dropped from output GEOMETRYCOLLECTION objects to simplify output if their topological dinension is less than the minimum topological dinension of the input objects.}
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
pol <- readWKT(paste("POLYGON((-180 -20, -140 55, 10 0, -140 -60, -180 -20),",
 "(-150 -20, -100 -10, -110 20, -150 -20))"))
library(sp)
GT <- GridTopology(c(-175, -85), c(10, 10), c(36, 18))
gr <- as(as(SpatialGrid(GT), "SpatialPixels"), "SpatialPolygons")
try(res <- gIntersection(pol, gr, byid=TRUE))
res <- gIntersection(pol, gr, byid=TRUE, drop_lower_td=TRUE)
# Robert Hijmans difficult intersection case
load(system.file("test_cases/polys.RData", package="rgeos"))
try(res <- gIntersection(a, b, byid=TRUE))
res <- gIntersection(a, b, byid=TRUE, drop_lower_td=TRUE)
unlist(sapply(slot(res, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area")))
oT <- get_RGEOS_polyThreshold()
oW <- get_RGEOS_warnSlivers()
oD <- get_RGEOS_dropSlivers()
set_RGEOS_polyThreshold(1e-3)
set_RGEOS_warnSlivers(TRUE)
res1 <- gIntersection(a, b, byid=TRUE, drop_lower_td=TRUE)
unlist(sapply(slot(res1, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area")))
set_RGEOS_dropSlivers(TRUE)
res2 <- gIntersection(a, b, byid=TRUE, drop_lower_td=TRUE)
unlist(sapply(slot(res2, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area")))
set_RGEOS_dropSlivers(FALSE)
oo <- gUnaryUnion(res1, c(rep("1", 3), "2", "3", "4"))
unlist(sapply(slot(oo, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area")))
ooo <- gIntersection(b, oo, byid=TRUE)
gArea(ooo, byid=TRUE)
unlist(sapply(slot(ooo, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area")))
set_RGEOS_dropSlivers(TRUE)
ooo <- gIntersection(b, oo, byid=TRUE)
gArea(ooo, byid=TRUE)
unlist(sapply(slot(ooo, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area")))
set_RGEOS_polyThreshold(oT)
set_RGEOS_warnSlivers(oW)
set_RGEOS_dropSlivers(oD)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}


