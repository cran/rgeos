\name{bbox2SP}
\alias{bbox2SP}
\title{Converts a bounding box into a SpatialPolygons object.}
\description{Converts a bounding box into a SpatialPolygons object.}

\usage{bbox2SP(n,s,w,e,bbox=NA,proj4string=CRS("+init=epsg:4326"))}

\arguments{
  \item{n}{the top north latitude}
  \item{s}{the bottom south latitude}
  \item{w}{the most western longitude}
  \item{e}{the most eastern longitude}
  \item{bbox}{a bounding box 2 x 2 matrix as produced by  \code{\link[sp]{bbox}}}
  \item{proj4string}{a coordinate reference system as defined in  \code{\link[sp]{CRS}}}
}

\details{

This function converts a set of coordinates limiting a bounding box into a SpatialPolygons. It can be used for instance to clip a subset of a larger spatial object (e.g. using \code{\link[rgeos]{gIntersection}})
}

\value{ 
An object of \code{\link[sp]{SpatialPolygons}} class.
}


\examples{


if (require(rgdal)) {
cities <- readOGR(dsn=system.file("vectors", package = "rgdal")[1], layer="cities")
n<-75
s<-30
w<--40
e<-32
myPoly<-bbox2SP(n,s,e,w)
plot(cities)
plot(myPoly,border="red",add=TRUE)

bb<-bbox(cities)
myPoly<-bbox2SP(bbox=bb,proj4string=CRS(proj4string(cities)))
plot(myPoly,add=TRUE,border="blue")
}


}




\keyword{utilities}

