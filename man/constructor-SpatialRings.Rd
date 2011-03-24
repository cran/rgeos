\name{SpatialRings}
\alias{Ring}
\alias{SpatialRings}
\alias{SpatialRingsDataFrame}

\title{ create SpatialRings or SpatialRingsDataFrame}

\description{ create objects of class \code{SpatialRings} or \code{SpatialRingsDataFrame} }

\usage{ 
Ring(coords,ID=as.character(NA))
SpatialRings(RingList, proj4string=CRS(as.character(NA)))
SpatialRingsDataFrame(sr, data, match.ID = TRUE)
}

\arguments{
\item{coords}{2-column numeric matrix with coordinates; first point (row) should
equal last coordinates (row); if the hole argument is not given, the status of the polygon as a hole or an island will be taken from the ring direction, with clockwise meaning island, and counter-clockwise meaning hole}
\item{ID}{character vector of length one with identifier}
\item{RingList}{ list with objects of class \link{Ring-class}}
\item{proj4string}{Object of class \code{"CRS"} holding a valid proj4 string}
\item{sr}{ object of class \link{SpatialRings-class}}
\item{data}{ object of class \code{data.frame}; the number of rows in \code{data}
should equal the number of \code{Lines} elements in \code{sl}}
\item{match.ID}{logical: (default TRUE): match SpatialLines member Lines ID slot values with data frame row names, and re-order the data frame rows if necessary}
}

\value{
\code{Ring} returns object of class \code{Ring}
\code{SpatialRings} returns object of class \code{SpatialRings}
\code{SpatialRingsDataFrame} returns object of class \code{SpatialRingsDataFrame}
}
\seealso{ \link{Ring-class} \link{SpatialRings-class} \link{SpatialRingsDataFrame-class}  }

\keyword{manip}
