suppressPackageStartupMessages(library(rgeos))
nc34 <- readWKT(readLines(system.file("wkts/nc34.wkt", package="rgeos")))
xF <- gSimplify(nc34, tol=5, topologyPreserve=FALSE)
xT <- gSimplify(nc34, tol=5, topologyPreserve=TRUE)
# https://github.com/r-spatial/sf/issues/1655#issuecomment-826358383
