\name{RGEOS Experimental Functions}
\alias{poly_findInBoxGEOS}
\alias{gUnarySTRtreeQuery}
\alias{gBinarySTRtreeQuery}

\title{Experimental Functions}
\description{Functions still under development using the GEOS STRtree structure to find intersecting object component envelopes (bounding boxes).}
\usage{
    gUnarySTRtreeQuery(obj)
    gBinarySTRtreeQuery(obj1, obj2)
    poly_findInBoxGEOS(spl, as_points=TRUE)
}

\arguments{
  \item{obj, obj1, obj2}{Objects inheriting from either \code{SpatialPolygons} or \code{SpatialLines}, obj2 may also inherit from \code{SpatialPoints}}
  \item{spl}{Object that inherits from the \code{SpatialPolygons} class}
  \item{as_points}{Logical value indicating if the polygon should be treated as points}
}

\details{\code{gUnarySTRtreeQuery} and \code{poly_findInBoxGEOS} do the same thing, but \code{poly_findInBoxGEOS} uses the \code{as_points} argument to build the input envelopes from proper geometries. \code{gUnarySTRtreeQuery} and \code{gBinarySTRtreeQuery} build input envelopes by disregarding topology and reducing the coordinates to a multipoint representation. This permits the tree to be built and queried even when some geometries are invalid. \code{gUnarySTRtreeQuery} and \code{poly_findInBoxGEOS} return a list of length \code{(n-1)} of 1-based indices only for the \dQuote{greater than i} indices. \code{gBinarySTRtreeQuery} returns a list of the length of \code{obj2} with 1-based indices of \code{obj1}.
}

\author{Roger Bivand & Colin Rundel}

\keyword{spatial}

\examples{
if (require(maptools)) {
xx <- readShapeSpatial(system.file("shapes/fylk-val-ll.shp",
 package="maptools")[1], proj4string=CRS("+proj=longlat +datum=WGS84"))
a0 <- gUnarySTRtreeQuery(xx)
a0
nc1 <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
 proj4string=CRS("+proj=longlat +datum=NAD27"))
a2 <- gUnarySTRtreeQuery(nc1)
#a3 <- poly_findInBoxGEOS(nc1)
#all.equal(a2, a3)
a2
pl <- slot(nc1, "polygons")[[4]]
a5 <- gUnarySTRtreeQuery(pl)
a5
SG <- Sobj_SpatialGrid(nc1, n=400)$SG
obj1 <- as(as(SG, "SpatialPixels"), "SpatialPolygons")
a4 <- gBinarySTRtreeQuery(nc1, obj1)
plot(nc1, col="orange", border="yellow")
plot(obj1, angle=sapply(a4, is.null)*45, density=20, lwd=0.5, add=TRUE)
set.seed(1)
pts <- spsample(nc1, n=10, type="random")
res <- gBinarySTRtreeQuery(nc1, pts)
}
}
