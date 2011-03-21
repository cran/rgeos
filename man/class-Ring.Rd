\name{Ring-class}
\docType{class}
\alias{Ring-class}
\alias{bbox,Ring-method}
\alias{coordinates,Ring-method}
\alias{coordnames,Ring-method}
\alias{coordnames<-,Ring,character-method}
\alias{coerce,Ring,SpatialPoints-method}



\title{Class "Ring"}
\description{  class for linear ring }
\section{Objects from the Class}{
Objects can be created by calls to the function \link{Ring}
}
\section{Slots}{
  \describe{
    \item{\code{coords}:}{Object of class \code{"matrix"}; coordinates of the ring;
	first point should equal the last point }
    \item{\code{ID}:}{Object of class \code{"character"}; unique identifier string  }
  }
}

\section{Methods}{
Methods defined with class "Ring" in the signature:
  \describe{
    \item{bbox}{\code{signature(obj = "Ring")}: retrieves the bbox element }
	\item{coordinates}{\code{signature(object = "Ring")}: retrieves the coords element from Ring objects in rings slot}
	\item{coordnames}{\code{signature(object = "Ring")}: retrieves coordinate names}
	\item{coerce}{\code{signature(from = "Ring", to = "SpatialPoints")}: ... }
  }
}

\author{ Colin Rundel }
\seealso{ \link{Ring} }

\examples{ 
	#NONE 
}

\keyword{classes}
