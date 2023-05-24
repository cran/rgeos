\name{rgeos-deprecated}
\alias{rgeos-deprecated}
\alias{gBuffer}
\alias{RGEOSBuffer}
\alias{gDifference}
\alias{gIntersection}
\alias{gSymdifference}
\alias{gUnion}
\title{Deprecated functions and methods}
\description{Cumulative deprecated functions and methods from rgeos prior to package retirement/archiving during 2023.}
\usage{
gBuffer(spgeom, byid=FALSE, id=NULL, width=1.0, quadsegs=5, capStyle="ROUND",
 joinStyle="ROUND", mitreLimit=1.0)
gDifference(spgeom1, spgeom2, byid=FALSE, id=NULL, drop_lower_td=FALSE,
 unaryUnion_if_byid_false=TRUE, checkValidity=NULL)
gSymdifference(spgeom1, spgeom2, byid=FALSE, id=NULL, drop_lower_td=FALSE,
 unaryUnion_if_byid_false=TRUE, checkValidity=NULL)
gIntersection(spgeom1, spgeom2, byid=FALSE, id=NULL, drop_not_poly,
 drop_lower_td=FALSE, unaryUnion_if_byid_false=TRUE, checkValidity=NULL)
gUnion(spgeom1, spgeom2, byid=FALSE, id=NULL, drop_lower_td=FALSE,
 unaryUnion_if_byid_false=TRUE, checkValidity=NULL)
}
\arguments{
  \item{spgeom}{sp object as defined in package sp}
  \item{byid}{Logical determining if the function should be applied across subgeometries (TRUE) or the entire object (FALSE)}
  \item{id}{Character vector defining id labels for the resulting geometries, if unspecified returned geometries will be labeled based on their parent geometries' labels.}
  \item{width}{Distance from original geometry to include in the new geometry. Negative values are allowed. Either a numeric vector of length 1 when byid is FALSE; if byid is TRUE: of length 1 replicated to the number of input geometries, or of length equal to the number of input geometries}
  \item{quadsegs}{Number of line segments to use to approximate a quarter circle.}
  \item{capStyle}{Style of cap to use at the ends of the geometry. Allowed values: \code{ROUND},\code{FLAT},\code{SQUARE}}
  \item{joinStyle}{Style to use for joints in the geometry. Allowed values: \code{ROUND},\code{MITRE},\code{BEVEL}}
  \item{mitreLimit}{Numerical value that specifies how far a joint can extend if a mitre join style is used.}
  \item{spgeom1, spgeom2}{sp objects as defined in package sp}
  \item{drop_lower_td}{default FALSE; if TRUE, objects will be dropped from output GEOMETRYCOLLECTION objects to simplify output if their topological dinension is less than the minimum topological dinension of the input objects.}
  \item{unaryUnion_if_byid_false}{default TRUE; if \code{byid} takes a FALSE in either position, the subgeometries are combined first to avoid possible topology exceptions (change in 0.3-13, previous behaviour did not combine subgeometries, and may be achieved by setting this argument FALSE}
  \item{checkValidity}{default NULL, integer 0L (no action), 1L (check), 2L (check and try to buffer by zero distance to repair). If NULL, a value set to 0L for GEOS < 3.7.2, 1L for GEOS >= 3.7.2 is read from values assigned on load. Error meesages from GEOS do not say clearly which object fails if a topology exception is encountered. If this argument is > 0L, \code{gIsValid} is run on each in turn}
  \item{drop_not_poly}{deprecated argument, use drop_lower_td}
}
\examples{
p1 = readWKT("POLYGON((0 1,0.95 0.31,0.59 -0.81,-0.59 -0.81,-0.95 0.31,0 1))")
p2 = readWKT("POLYGON((2 2,-2 2,-2 -2,2 -2,2 2),(1 1,-1 1,-1 -1,1 -1,1 1))")

par(mfrow=c(2,3))
plot(gBuffer(p1,width=-0.2),col='black',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
plot(p1,border='blue',lwd=2,add=TRUE);title("width: -0.2")
plot(gBuffer(p1,width=0),col='black',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
plot(p1,border='blue',lwd=2,add=TRUE);title("width: 0")
plot(gBuffer(p1,width=0.2),col='black',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
plot(p1,border='blue',lwd=2,add=TRUE);title("width: 0.2")

plot(gBuffer(p2,width=-0.2),col='black',pbg='white',xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p2,border='blue',lwd=2,add=TRUE);title("width: -0.2")
plot(gBuffer(p2,width=0),col='black',pbg='white',xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p2,border='blue',lwd=2,add=TRUE);title("width: 0")
plot(gBuffer(p2,width=0.2),col='black',pbg='white',xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p2,border='blue',lwd=2,add=TRUE);title("width: 0.2")

p3 <- readWKT(paste("GEOMETRYCOLLECTION(",
 "POLYGON((0 1,0.95 0.31,0.59 -0.81,-0.59 -0.81,-0.95 0.31,0 1)),",
 "POLYGON((2 2,-2 2,-2 -2,2 -2,2 2),(1 1,-1 1,-1 -1,1 -1,1 1)))"))

par(mfrow=c(1,1))
plot(gBuffer(p3, byid=TRUE, width=c(-0.2, -0.1)),col='black',pbg='white',
xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p3,border=c('blue', 'red'),lwd=2,add=TRUE);title("width: -0.2, -0.1")
library(sp)
p3df <- SpatialPolygonsDataFrame(p3, data=data.frame(i=1:length(p3),
 row.names=row.names(p3)))
dim(p3df)
row.names(p3df)
dropEmpty = gBuffer(p3df, byid=TRUE, id=letters[1:nrow(p3df)], width=c(-1, 0))
dim(dropEmpty)
row.names(dropEmpty)
row.names(slot(dropEmpty, "data"))
plot(dropEmpty, col='black', pbg='white', xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p3df,border=c('blue'),lwd=2,add=TRUE);title("width: -1, 0")
par(mfrow=c(2,3))


#Style options
l1 = readWKT("LINESTRING(0 0,1 5,4 5,5 2,8 2,9 4,4 6.5)")
par(mfrow=c(2,3))
plot(gBuffer(l1,capStyle="ROUND"));plot(l1,col='blue',add=TRUE);title("capStyle: ROUND")
plot(gBuffer(l1,capStyle="FLAT"));plot(l1,col='blue',add=TRUE);title("capStyle: FLAT")
plot(gBuffer(l1,capStyle="SQUARE"));plot(l1,col='blue',add=TRUE);title("capStyle: SQUARE")

plot(gBuffer(l1,quadsegs=1));plot(l1,col='blue',add=TRUE);title("quadsegs: 1")
plot(gBuffer(l1,quadsegs=2));plot(l1,col='blue',add=TRUE);title("quadsegs: 2")
plot(gBuffer(l1,quadsegs=5));plot(l1,col='blue',add=TRUE);title("quadsegs: 5")



l2 = readWKT("LINESTRING(0 0,1 5,3 2)")
par(mfrow=c(2,3))
plot(gBuffer(l2,joinStyle="ROUND"));plot(l2,col='blue',add=TRUE);title("joinStyle: ROUND")
plot(gBuffer(l2,joinStyle="MITRE"));plot(l2,col='blue',add=TRUE);title("joinStyle: MITRE")
plot(gBuffer(l2,joinStyle="BEVEL"));plot(l2,col='blue',add=TRUE);title("joinStyle: BEVEL")

plot(gBuffer(l2,joinStyle="MITRE",mitreLimit=0.5));plot(l2,col='blue',add=TRUE)
 title("mitreLimit: 0.5")
plot(gBuffer(l2,joinStyle="MITRE",mitreLimit=1));plot(l2,col='blue',add=TRUE)
 title("mitreLimit: 1")
plot(gBuffer(l2,joinStyle="MITRE",mitreLimit=3));plot(l2,col='blue',add=TRUE)
 title("mitreLimit: 3")
x = readWKT("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
y = readWKT("POLYGON ((3 3, 7 3, 7 7, 3 7, 3 3))")

d = gDifference(x,y)
plot(d,col='red',pbg='white')

# Empty geometry since y is completely contained with x
d2 = gDifference(y,x)
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
x = readWKT("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
y = readWKT("POLYGON ((5 5, 15 5, 15 15, 5 15, 5 5))")

d = gSymdifference(x,y)
plot(d,col='red',pbg='white')
}
