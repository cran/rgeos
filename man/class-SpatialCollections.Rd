\name{SpatialCollections-class}
\docType{class}
\alias{SpatialCollections-class}
\alias{SpatialLinesNULL-class}
\alias{SpatialRingsNULL-class}
\alias{SpatialPolygonsNULL-class}
\alias{SpatialPointsNULL-class}
\alias{row.names,SpatialCollections-method}
\alias{plot,SpatialCollections,missing-method}


\title{Class "SpatialCollections"}
\description{  class to hold SpatialPoints, SpatialLines, SpatialRings, and SpatialPolygons (without attributes) }
\section{Objects from the Class}{
Objects can be created by calls to the function \link{SpatialCollections}
}
\section{Slots}{
  \describe{
	\item{\code{pointobj}:}{Object of class SpatialPoints or NULL}
    \item{\code{lineobj}:}{Object of class SpatialLines or NULL}
	\item{\code{ringobj}:}{Object of class SpatialRings or NULL}
	\item{\code{polyobj}:}{Object of class SpatialPolygons or NULL}
	\item{\code{plotOrder}:}{Numeric vector of length 4}
  }
}

\section{Extends}{
	Class \code{"Spatial"}, directly.
}

\section{Methods}{
Methods defined with class "SpatialCollections" in the signature:
  \describe{
	\item{plot}{\code{signature(x = "SpatialCollections", y = "missing")}: plot objects within the SpatialCollections object in the order specified by plotOrder slot}
	\item{row.names}{\code{signature(object = "SpatialCollections")}: retrieves the ID elements from non-NULL geometry slots}
  }
}

\author{ Colin Rundel }
\seealso{ \link{SpatialCollections} \link{SpatialPoints} \link{SpatialLines} \link{SpatialRings} \link{SpatialPolygons} }

\examples{ 
	#NONE 
}

\keyword{classes}
