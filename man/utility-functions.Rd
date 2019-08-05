\name{RGEOS Utility Functions}
\alias{getScale}
\alias{setScale}
\alias{translate}
\alias{checkP4S}
\alias{version_GEOS}
\alias{version_GEOS0}
\alias{set_RGEOS_polyThreshold}
\alias{get_RGEOS_polyThreshold}
\alias{set_RGEOS_warnSlivers}
\alias{get_RGEOS_warnSlivers}
\alias{set_RGEOS_dropSlivers}
\alias{get_RGEOS_dropSlivers}
\alias{set_RGEOS_CheckValidity}
\alias{get_RGEOS_CheckValidity}
\alias{rgeos_extSoftVersion}

\title{RGEOS Utility Functions}
\description{Utility functions for the RGEOS package}
\usage{
	getScale()
	setScale(scale=100000000)
	translate(spgeom)
	checkP4S(p4s)
        version_GEOS(runtime = TRUE)
        rgeos_extSoftVersion()
        version_GEOS0()
        set_RGEOS_polyThreshold(value)
        get_RGEOS_polyThreshold()
        set_RGEOS_warnSlivers(value)
        get_RGEOS_warnSlivers()
        set_RGEOS_dropSlivers(value)
        get_RGEOS_dropSlivers()
        get_RGEOS_CheckValidity()
        set_RGEOS_CheckValidity(value)
}

\arguments{
  \item{scale}{Numeric value determining the precision of translated geometries}
  \item{spgeom}{sp object as defined in package sp}
  \item{p4s}{Either a character string or an object of class \code{CRS}}
  \item{runtime}{default TRUE; if FALSE, installation GEOS version}
  \item{value}{the value to be passed to an RGEOS option in its environment}
}

\details{getScale and setScale are used to get and set the scale option in the rgeos environment. This option is used to determine the precision of coordinates when translating to and from GEOS C objects. Precision is defined as 1 / scale. The final example is a use case reported by Mao-Gui Hu, who has also made the objects available, where the default scale defeats an intended line intersection operation; changing the scale temporarily resoves the issue.

In order to permit polygon slivers to be detected, reported and dropped, the user may set a numeric value for polyThreshold and logical values for warnSlivers and dropSlivers. By default, the threshold is 0.0, and warning and dropping are FALSE. To detect slivers, the threshold may be set to a small value and warnSlivers set to TRUE. To drop slivers from returned Polygons and Polygon objects, set dropSlivers to TRUE for a non-zero threshold.

translate is a testing function which translates the sp object into a GEOS C object and then back into an sp object and is used extensively in the translation unit tests. In all cases it is expected that \code{spgeom} and \code{translate(spgeom)} should be identical.

checkP4S is a validation function for proj4strings and is used in testing.

version_GEOS returns the full runtime version string, and version_GEOS0 only the GEOS version number.
set_RGEOS_CheckValidity takes an integer 0:2, 0L is no operation, 1L is check validity, and 2L is check and if invalid try to repair with a zero width buffer.
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
get_RGEOS_dropSlivers()
get_RGEOS_warnSlivers()
get_RGEOS_polyThreshold()
# Robert Hijmans difficult intersection case
load(system.file("test_cases/polys.RData", package="rgeos"))
try(res <- gIntersection(a, b, byid=TRUE))
res <- gIntersection(a, b, byid=TRUE, drop_lower_td=TRUE)
sort(unlist(sapply(slot(res, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area"))))
oT <- get_RGEOS_polyThreshold()
oW <- get_RGEOS_warnSlivers()
oD <- get_RGEOS_dropSlivers()
set_RGEOS_polyThreshold(1e-3)
set_RGEOS_warnSlivers(TRUE)
res1 <- gIntersection(a, b, byid=TRUE, drop_lower_td=TRUE)
sort(unlist(sapply(slot(res, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area"))))
set_RGEOS_dropSlivers(TRUE)
res2 <- gIntersection(a, b, byid=TRUE, drop_lower_td=TRUE)
sort(unlist(sapply(slot(res, "polygons"), function(p) sapply(slot(p, "Polygons"), slot, "area"))))
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

