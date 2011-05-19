overGeomGeom = function(x, y, returnList = FALSE, fn = NULL, ...) {
	stopifnot(identical(proj4string(x), proj4string(y)))
	if (gridded(x))
		x = as(x, "SpatialPolygons")
	if (gridded(y))
		y = as(y, "SpatialPolygons")
	gI = gIntersects(y, x, byid = TRUE)
	if (returnList) {
		ret = apply(gI, 1, which)
		if (!is.list(ret)) {
			ret = lapply(1:ncol(ret), function(x) ret[,x])
			names(ret) = names(x)
		}
	} else
		ret = apply(gI, 1, function(x) which(x)[1])
	ret
}

# taken from: overDFGeneric in sp; 
# if modified here, consider modifying there as well!
overGeomGeomDF = function(x, y, returnList = FALSE, fn = NULL, ...) {
    r = overGeomGeom(x, y, returnList = TRUE)
    ret = sp:::.overDF(r, y@data, length(x), returnList, fn, ...)
    if (!returnList)
        row.names(ret) = row.names(x)
    ret
}

#setMethod("over",
#    signature(x = "SpatialPoints", y = "SpatialPolygons"),
#	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialPoints", y = "SpatialLines"),
	        overGeomGeom)
#setMethod("over",
#    signature(x = "SpatialPoints", y = "SpatialPoints"),
#	        overGeomGeom)
#setMethod("over",
#    signature(x = "SpatialPolygons", y = "SpatialPoints"),
#	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialPolygons", y = "SpatialLines"),
	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialPolygons", y = "SpatialPolygons"),
	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialPoints"),
	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialPolygons"),
	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialLines"),
	        overGeomGeom)

# all with DataFrame:
#setMethod("over",
#    signature(x = "SpatialPoints", y = "SpatialPolygonsDataFrame"),
#	        overGeomGeomDF)
setMethod("over",
    signature(x = "SpatialPoints", y = "SpatialLinesDataFrame"),
	        overGeomGeomDF)
#setMethod("over",
#    signature(x = "SpatialPoints", y = "SpatialPointsDataFrame"),
#	        overGeomGeomDF)
#setMethod("over",
#    signature(x = "SpatialPolygons", y = "SpatialPointsDataFrame"),
#	        overGeomGeomDF)
setMethod("over",
    signature(x = "SpatialPolygons", y = "SpatialLinesDataFrame"),
	        overGeomGeomDF)
setMethod("over",
    signature(x = "SpatialPolygons", y = "SpatialPolygonsDataFrame"),
	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialPointsDataFrame"),
	        overGeomGeomDF)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialPolygonsDataFrame"),
	        overGeomGeomDF)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialLinesDataFrame"),
	        overGeomGeomDF)

# lines & grids:
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialPixels"),
	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialGrid"),
	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialPixelsDataFrame"),
	        overGeomGeomDF)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialGridDataFrame"),
	        overGeomGeomDF)
setMethod("over",
    signature(x = "SpatialPixels", y = "SpatialLines"),
	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialGrid", y = "SpatialLinesDataFrame"),
	        overGeomGeomDF)
