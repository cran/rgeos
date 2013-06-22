\name{SpatialCollections}
\alias{SpatialCollections}

\title{ create SpatialCollections}

\description{ create object of class \code{SpatialCollections} }

\usage{ 
SpatialCollections(points=NULL, lines=NULL, rings=NULL, polygons=NULL,
 plotOrder=c(4,3,2,1), proj4string=CRS(as.character(NA)))
}

\arguments{
\item{points}{ list with objects of class \link{SpatialPoints-class}}
\item{lines}{ list with objects of class \link{SpatialLines-class}}
\item{rings}{ list with objects of class \link{SpatialRings-class}}
\item{polygons}{ list with objects of class \link{SpatialPolygons-class}}
\item{plotOrder}{ numeric vector of length 4 that determines the order in which the geometries will be plotted. By default polygons will be plotted followed by rings, then lines and finally points.}
\item{proj4string}{Object of class \code{"CRS"} holding a valid proj4 string}
}

\value{
\code{SpatialCollections} returns object of class \code{SpatialCollections}
}

\seealso{ \link{SpatialCollections-class} \link{SpatialPoints-class} \link{SpatialLines-class} \link{SpatialRings-class} \link{SpatialPolygons-class} }

\keyword{manip}
