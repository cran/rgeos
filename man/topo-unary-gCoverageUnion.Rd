\name{gCoverageUnion}
\alias{gCoverageUnion}

\title{Coverage Union}
\description{GEOSCoverageUnion is an optimized union algorithm for polygonal inputs that are correctly noded and do not overlap. It will not generate an error (return NULL) for inputs that do not satisfy this constraint.}
\usage{
gCoverageUnion(spgeom, byid=FALSE, id = NULL)
}

\arguments{
  \item{spgeom}{sp object as defined in package sp}
  \item{byid}{Logical determining if the function should be applied across subgeometries (TRUE) or the entire object (FALSE)}
  \item{id}{Character vector defining id labels for the resulting geometries, if unspecified returned geometries will be labeled based on their parent geometries' labels.}
}


\author{Roger Bivand}

\seealso{
\code{\link{gUnaryUnion}}
}

\examples{
run <- FALSE
if (require(maptools)) run <- TRUE
if (run) {
    nc1 <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
        proj4string=CRS("+proj=longlat +datum=NAD27"))
}
if (run) {
    print(system.time(oU <- gUnionCascaded(nc1)))
    if (version_GEOS0() >= "3.8.0") {
        print(system.time(oCU <- gCoverageUnion(nc1)))
    }
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

