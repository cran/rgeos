\name{SpatialRings-class}
\docType{class}
\alias{SpatialRings-class}
\alias{[,SpatialRings-method}
\alias{plot,SpatialRings,missing-method}
\alias{coordinates,SpatialRings-method}
\alias{coordnames,SpatialRings-method}
\alias{coordnames<-,SpatialRings,character-method}
\alias{row.names,SpatialRings-method}
\alias{row.names<-,SpatialRings,character-method}
\alias{spChFIDs,SpatialRings,character-method}
\alias{coerce,SpatialRings,SpatialPoints-method}



\title{Class "SpatialRings"}
\description{  class to hold linear ring topology (without attributes) }
\section{Objects from the Class}{
Objects can be created by calls to the function \link{SpatialRings}
}
\section{Slots}{
  \describe{
    \item{\code{rings}:}{Object of class \code{"list"}; list elements are
	all of class \link{Ring-class}}
    \item{\code{bbox}:}{Object of class \code{"matrix"}; see \link{Spatial-class} }
    \item{\code{proj4string}:}{Object of class \code{"CRS"}; see \link{CRS-class}}
  }
}

\section{Extends}{
Class \code{"Spatial"}, directly.
}

\section{Methods}{
Methods defined with class "SpatialRings" in the signature:
  \describe{
    \item{[}{\code{signature(obj = "SpatialRings")}: select subset of (sets of) rings; NAs are not permitted in the row index}
	\item{plot}{\code{signature(x = "SpatialRings", y = "missing")}: plot rings in SpatialRings object}
	\item{bbox}{\code{signature(obj = "SpatialRings")}: retrieves the bbox element }
	\item{coordinates}{\code{signature(object = "SpatialRings")}: retrieves the coords element from Ring objects in rings slot}
	\item{coordnames}{\code{signature(object = "SpatialRings")}: retrieves coordinate names}
	\item{row.names}{\code{signature(object = "SpatialRings")}: retrieves the ID element from Ring objects in rings slot}
	\item{spChFIDs}{\code{signature(obj="SpatialRings", x="character")}: replaces ID element}
	\item{coerce}{\code{signature(from = "SpatialRings", to = "SpatialPoints")}: ... }
  }
}

\author{ Colin Rundel }
\seealso{ \link{SpatialRings} \link{Ring-class} }

\examples{ 
	#NONE 
}

\keyword{classes}
