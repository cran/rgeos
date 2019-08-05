\name{gIntersection}
\alias{gIntersection}
\title{Geometry Intersections}
\description{Function for determining the intersection between the two given geometries}
\usage{
gIntersection(spgeom1, spgeom2, byid=FALSE, id=NULL, drop_not_poly,
 drop_lower_td=FALSE, unaryUnion_if_byid_false=TRUE, checkValidity=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spgeom1, spgeom2}{sp objects as defined in package sp}
  \item{byid}{Logical vector determining if the function should be applied across ids (TRUE) or the entire object (FALSE) for spgeom1 and spgeom2}
  \item{id}{Character vector defining id labels for the resulting geometries, if unspecified returned geometries will be labeled based on their parent geometries' labels.}
  \item{drop_not_poly}{deprecated argument, use drop_lower_td}
  \item{drop_lower_td}{default FALSE; if TRUE, objects will be dropped from output GEOMETRYCOLLECTION objects to simplify output if their topological dimension is less than the minimum topological dinension of the input objects.}
  \item{unaryUnion_if_byid_false}{default TRUE; if \code{byid} takes a FALSE in either position, the subgeometries are combined first to avoid possible topology exceptions (change in 0.3-13, previous behaviour did not combine subgeometries, and may be achieved by setting this argument FALSE}
  \item{checkValidity}{default NULL, integer 0L (no action), 1L (check), 2L (check and try to buffer by zero distance to repair). If NULL, a value set to 0L for GEOS < 3.7.2, 1L for GEOS >= 3.7.2 is read from values assigned on load. Error meesages from GEOS do not say clearly which object fails if a topology exception is encountered. If this argument is > 0L, \code{gIsValid} is run on each in turn}
}

\details{Returns all spatial intersections as sp objects of the appropriate class. If the geometries do not intersect then an empty geometry is returned.}

\note{Error messages from GEOS, in particular topology exceptions, report 0-based object order, so geom 0 is spgeom1, and geom 1 is spgeom2.}

\author{Roger Bivand & Colin Rundel}

\seealso{
\code{\link{gDifference}}
\code{\link{gSymdifference}}
\code{\link{gUnion}}
}

\examples{
if (require(maptools)) {
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
# example from Pius Korner 2015-10-25
poly1 <- SpatialPolygons(list(Polygons(list(Polygon(coords=matrix(c(0, 0, 2, 2, 0, 1, 1, 0),
 ncol=2, byrow=FALSE))), ID=c("a")), Polygons(list(Polygon(coords=matrix(c(0, 0, 2, 2, 2, 3, 3, 2),
 ncol=2, byrow=FALSE))), ID=c("b"))))
poly2 <- SpatialPolygons(list(Polygons(list(Polygon(coords=matrix(c(0, 0, 2, 2,
 1, 1, 1, 3, 3, 0, 0, 2), ncol=2, byrow=FALSE))), ID=c("c"))))
plot(poly1, border="orange")
plot(poly2, border="blue", add=TRUE, lty=2, density=8, angle=30, col="blue")
gI <- gIntersection(poly1, poly2, byid=TRUE, drop_lower_td=TRUE)
plot(gI, add=TRUE, border="red", lwd=3)
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
oo <- gUnaryUnion(res1, c(rep("1", 3), "2", "3", "4"), checkValidity=2L)
unlist(sapply(slot(oo, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area")))
ooo <- gIntersection(b, oo, byid=TRUE, checkValidity=2L)
gArea(ooo, byid=TRUE)
unlist(sapply(slot(ooo, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area")))
set_RGEOS_dropSlivers(TRUE)
ooo <- gIntersection(b, oo, byid=TRUE, checkValidity=2L)
gArea(ooo, byid=TRUE)
unlist(sapply(slot(ooo, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area")))
set_RGEOS_polyThreshold(oT)
set_RGEOS_warnSlivers(oW)
set_RGEOS_dropSlivers(oD)
# see thread https://stat.ethz.ch/pipermail/r-sig-geo/2015-September/023468.html
Pol1=rbind(c(0,0),c(0,10),c(10,10),c(10,0),c(0,0))
Pol2=rbind(c(0,0),c(10,0),c(10,-10),c(0,-10),c(0,0))
library(sp)
Pols1=Polygons(list(Polygon(Pol1)),"Pols1")
Pols2=Polygons(list(Polygon(Pol2)),"Pols2")
MyLay=SpatialPolygons(list(Pols1,Pols2))
Pol1l=Pol1+0.5
Pol2l=Pol2+0.5
Pols1l=Polygons(list(Polygon(Pol1l)),"Pols1l")
Pols2l=Polygons(list(Polygon(Pol2l)),"Pols2l")
MyLayl=SpatialPolygons(list(Pols1l,Pols2l))
inter=gIntersection(MyLay, MyLayl)
plot(MyLay)
plot(MyLayl,add=TRUE)
plot(inter, add=TRUE, border="green")
try(gIntersection(MyLay, MyLayl, unaryUnion_if_byid_false=FALSE))
Pol1=rbind(c(0,0),c(0,1),c(1,1),c(1,0),c(0,0))
Pol2=rbind(c(0,0),c(1,0),c(1,-1),c(0,-1),c(0,0))
Pols1=Polygons(list(Polygon(Pol1)),"Pols1")
Pols2=Polygons(list(Polygon(Pol2)),"Pols2")
MyLay=SpatialPolygons(list(Pols1,Pols2))
Pol1l=Pol1+0.1
Pol2l=Pol2+0.1
Pols1l=Polygons(list(Polygon(Pol1l)),"Pols1l")
Pols2l=Polygons(list(Polygon(Pol2l)),"Pols2l")
MyLayl=SpatialPolygons(list(Pols1l,Pols2l))
inter=gIntersection(MyLay, MyLayl, unaryUnion_if_byid_false=FALSE)
gEquals(inter, MyLay)
inter1=gIntersection(MyLay, MyLayl, unaryUnion_if_byid_false=TRUE)
gEquals(inter1, MyLay)
gEquals(inter, inter1)
plot(MyLay, ylim=c(-1, 1.1))
plot(MyLayl, add=TRUE)
plot(inter, angle=45, density=10, add=TRUE)
plot(inter1, angle=135, density=10, add=TRUE)
inter2=gIntersection(MyLay, MyLayl)
gEquals(inter2, MyLay)
gEquals(inter1, inter2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}


