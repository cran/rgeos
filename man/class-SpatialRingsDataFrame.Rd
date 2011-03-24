\name{SpatialRingsDataFrame-class}
\docType{class}
\alias{SpatialRingsDataFrame-class}
\alias{[,SpatialRingsDataFrame-method}
\alias{plot,SpatialRingsDataFrame-method}
\alias{bbox,SpatialRingsDataFrame-method}
\alias{coordinates,SpatialRingsDataFrame-method}
\alias{coordnames,SpatialRingsDataFrame-method}
\alias{coordnames<-,SpatialRingsDataFrame,character-method}
\alias{row.names,SpatialRingsDataFrame-method}
\alias{row.names<-,SpatialRingsDataFrame,character-method}

\alias{names,SpatialRingsDataFrame-method}
\alias{names<-,SpatialRingsDataFrame,character-method}

\alias{dim,SpatialRingsDataFrame-method}

\alias{spChFIDs,SpatialRingsDataFrame,character-method}
\alias{coerce,SpatialRingsDataFrame,SpatialPoints-method}

\alias{coerce,SpatialRingsDataFrame,SpatialRings-method}
\alias{coerce,SpatialRingsDataFrame,data.frame-method}


\title{Class "SpatialRingsDataFrame"}
\description{  class to hold linear ring topology (without attributes) }
\section{Objects from the Class}{
Objects can be created by calls to the function \link{SpatialRingsDataFrame}
}
\section{Slots}{
  \describe{
	\item{\code{data}:}{Object of class \code{"data.frame"}; attribute table }
    \item{\code{rings}:}{Object of class \code{"list"}; list elements are
	all of class \link{Ring-class}}
    \item{\code{bbox}:}{Object of class \code{"matrix"}; see \link{Spatial-class} }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; see \link{CRS-class}}
  }
}

\section{Extends}{
Class \code{"SpatialRings"}, directly.
Class \code{"Spatial"}, by class \code{"SpatialRings"}.
}

\section{Methods}{
Methods defined with class "SpatialRingsDataFrame" in the signature:
  \describe{
    \item{[}{\code{signature(obj = "SpatialRingsDataFrame")}: select subset of (sets of) rings; NAs are not permitted in the row index}
	\item{plot}{\code{signature(x = "SpatialRingsDataFrame", y = "missing")}: plot rings in SpatialRingsDataFrame object}
	\item{bbox}{\code{signature(obj = "SpatialRingsDataFrame")}: retrieves the bbox element }
	\item{coordinates}{\code{signature(object = "SpatialRingsDataFrame")}: retrieves the coords element from Ring objects in rings slot}
	\item{coordnames}{\code{signature(object = "SpatialRingsDataFrame")}: retrieves coordinate names}
	\item{row.names}{\code{signature(object = "SpatialRingsDataFrame")}: retrieves the ID element from Ring objects in rings slot}
	\item{spChFIDs}{\code{signature(obj="SpatialRingsDataFrame", x="character")}: replaces ID element}
	
	\item{names}{\code{signature(object = "SpatialRingsDataFrame")}: retrieves names from data element}
	\item{dim}{\code{signature(object = "SpatialRingsDataFrame")}: retrieves dimensions of data element}
	
	\item{coerce}{\code{signature(from = "SpatialRingsDataFrame", to = "SpatialPoints")}: ... }
	\item{coerce}{\code{signature(from = "SpatialRingsDataFrame", to = "SpatialRings")}: ... }
	\item{coerce}{\code{signature(from = "SpatialRingsDataFrame", to = "data.frame")}: ... }
  }
}

\author{ Colin Rundel }
\seealso{ \link{SpatialRingsDataFrame} \link{Ring-class} \link{SpatialRings-class} }

\examples{ 
	#NONE 
}

\keyword{classes}
