\name{RGEOS Utility Functions}
\alias{getScale}
\alias{setScale}
\alias{translate}
\alias{checkP4S}
\alias{version_GEOS}
\alias{version_GEOS0}

\title{RGEOS Utility Functions}
\description{Utility functions for the RGEOS package}
\usage{
	getScale()
	setScale(scale=100000000)
	translate(spgeom)
	checkP4S(p4s)
        version_GEOS()
        version_GEOS0()
}

\arguments{
  \item{scale}{Numeric value determining the precision of translated geometries}
  \item{spgeom}{sp object as defined in package sp}
  \item{p4s}{Either a character string or an object of class \code{CRS}}
}

\details{getScale and setScale are used to get and set the scale option in the rgeos environment. This option is used to determine the precision of coordinates when translating to and from GEOS C objects. Precision is defined as 1 / scale. The final example is a use case reported by Mao-Gui Hu, who has also made the objects available, where the default scale defeats an intended line intersection operation; changing the scale temporarily resoves the issue.

translate is a testing function which translates the sp object into a GEOS C object and then back into an sp object and is used extensively in the translation unit tests. In all cases it is expected that \code{spgeom} and \code{translate(spgeom)} should be identical.

checkP4S is a validation function for proj4strings and is used in testing.

version_GEOS returns the full runtime version string, and version_GEOS0 only the GEOS version number.
}

\author{Roger Bivand & Colin Rundel}


\examples{
	readWKT("POINT(1.5 1.5)")

	# With scale set to 1, the following point will be rounded
	setScale(1)
	readWKT("POINT(1.5 1.5)")
	
	setScale(10)
	readWKT("POINT(1.5 1.5)")
	
	getScale()
	# Set scale option back to default
	setScale()
	
	
	
	# scale option only affect objects when they are translated through rgeos
	setScale(1)
	library(sp)
	SpatialPoints(data.frame(x=1.5,y=1.5))
	translate( SpatialPoints(data.frame(x=1.5,y=1.5)) )
	
	setScale()

        # added example of scale impact on intersection 120905
sline1 <- readWKT(readLines(system.file("wkts/sline1.wkt", package="rgeos")))
sline2 <- readWKT(readLines(system.file("wkts/sline2.wkt", package="rgeos")))
rslt <- gIntersection(sline1, sline2)
class(rslt)
getScale()
setScale(1e+6)
rslt <- gIntersection(sline1, sline2)
class(rslt)
sapply(slot(rslt, "lines"), function(x) length(slot(x, "Lines")))
rslt <- gLineMerge(rslt, byid=TRUE)
sapply(slot(rslt, "lines"), function(x) length(slot(x, "Lines")))
setScale()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

