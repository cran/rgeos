gSimplify = function(spgeom, tol, topologyPreserve=FALSE) {

	getCutEdges = as.logical(topologyPreserve)
	if (is.na(topologyPreserve))
		stop("Invalid value for topologyPreserve, must be logical")
	
    if (inherits(spgeom, "SpatialPolygons") && get_do_poly_check()) 
        spgeom <- createSPComment(spgeom)
    id = row.names(spgeom)
    return( .Call("rgeos_simplify", .RGEOS_HANDLE, spgeom, tol, id, 
									FALSE, topologyPreserve, PACKAGE="rgeos") )
}

gPolygonize = function( splist, getCutEdges=FALSE) {
	
	if (!is.list(splist))
		splist = list(splist)

	p4slist = lapply(splist,function(x) x@proj4string)
        splist <- lapply(splist, function(s) {
            if (inherits(s, "SpatialPolygons") && get_do_poly_check()) {
                createSPComment(s)
            } else {
                s
            }
        })
	
	p4s = p4slist[[1]]
	if (length(p4slist) != 1) {
		for(i in 2:length(p4slist)) {
			if (!identical(p4s, p4slist[[i]]))
				stop("spgeoms in splist have different proj4strings")
		}
	}
	

	getCutEdges = as.logical(getCutEdges)
	if (is.na(getCutEdges))
		stop("Invalid value for getCutEdges, must be logical")
	
	nid = sum(sapply(list(splist),function(x) length(unlist(row.names(x)))))
    id = as.character(1:nid)

    return( .Call("rgeos_polygonize", .RGEOS_HANDLE, splist, id, p4s,
									getCutEdges, PACKAGE="rgeos") )
}





TopologyFunc = function(spgeom, id, byid, func) {
    
    byid = as.logical(byid)
    if (is.na(byid)) 
        stop("Invalid value for byid, must be logical")
    
    curids = unique(row.names(spgeom))
    if (is.null(id)) {
        if (byid)   id = curids
        else        id = "1"
    }
    id = as.character(id)

    if (inherits(spgeom, "SpatialPolygons") && get_do_poly_check()) spgeom <- createSPComment(spgeom)
    
    if ( length(id) != length(unique(id)) )
        stop("Non-unique values for id ")
    
    if ( !(!byid && length(id) == 1) && !(byid && length(id) == length(curids)) )
        stop("Invalid number of values in id" ) 
    
    return( .Call(func, .RGEOS_HANDLE, spgeom, id, byid, PACKAGE="rgeos") )
}

gEnvelope = function(spgeom, byid=FALSE, id = NULL) {
    return( TopologyFunc(spgeom,id,byid,"rgeos_envelope") ) 
}
gConvexHull = function(spgeom, byid=FALSE, id = NULL) {
    return( TopologyFunc(spgeom,id,byid,"rgeos_convexhull") ) 
}
gBoundary = function(spgeom, byid=FALSE, id = NULL) {
     return( TopologyFunc(spgeom,id,byid,"rgeos_boundary") ) 
}
gCentroid = function(spgeom, byid=FALSE, id = NULL) {
    return( TopologyFunc(spgeom,id,byid,"rgeos_getcentroid") ) 
}
gPointOnSurface = function(spgeom, byid=FALSE, id = NULL) {
    return( TopologyFunc(spgeom,id,byid,"rgeos_pointonsurface") ) 
}
gLineMerge = function(spgeom, byid=FALSE, id = NULL) {
    return( TopologyFunc(spgeom,id,byid,"rgeos_linemerge") ) 
}

gUnionCascaded = function(spgeom, id = NULL, bound = 1000L) {
    
    if (!inherits(spgeom,"SpatialPolygons"))
        stop("Invalid geometry, may only be applied to polygons")
    spgeom <- as(spgeom, "SpatialPolygons")

    if (is.null(id))
        id = rep("1",length(row.names(spgeom)))

    if (any(is.na(id))) stop("No NAs permitted in id")

    if (get_do_poly_check()) spgeom <- createSPComment(spgeom)

    ids <- split(1:length(id), id)
    sl <- sapply(ids, length)
    if (any(sl > bound)) {
        stop(paste("Too many polygons in group to dissolve:",
            paste(sl, collapse=","), "greater than", bound,
            "- see help page", ifelse(version_GEOS0() < "3.3.0", "",
            "- consider using gUnaryUnion instead")))
    }
    out <- vector(mode="list", length=length(ids))
    for (i in seq(along=ids)) {
        out[[i]] <- TopologyFunc(groupID(spgeom[ids[[i]]], id[ids[[i]]]),
            names(ids)[i], TRUE, "rgeos_unioncascaded")
    }
    res <- do.call("rbind.SpatialPolygons", out)

    res
}

gUnaryUnion = function(spgeom, id = NULL) {

    if (version_GEOS0() < "3.3.0")
        stop("No UnaryUnion in this version of GEOS")
    
    if (!inherits(spgeom,"SpatialPolygons"))
        stop("Invalid geometry, may only be applied to polygons")
    spgeom <- as(spgeom, "SpatialPolygons")
    if (is.null(id))
        id = rep("1",length(row.names(spgeom)))

    if (any(is.na(id))) stop("No NAs permitted in id")

    if (get_do_poly_check()) spgeom <- createSPComment(spgeom)

    ids <- split(1:length(id), id)
    out <- vector(mode="list", length=length(ids))
    for (i in seq(along=ids)) {
        out[[i]] <- .Call("rgeos_unaryunion", .RGEOS_HANDLE,
        spgeom[ids[[i]]], names(ids)[i], FALSE, PACKAGE="rgeos") 
    }
    res <- do.call("rbind.SpatialPolygons", out)
    res
}


RGEOSEnvelope = function(spgeom, byid=FALSE, id = NULL) {
    .Deprecated("gEnvelope")
    return( gEnvelope(spgeom, id, byid) )
}
RGEOSConvexHull = function(spgeom, byid=FALSE, id = NULL) {
    .Deprecated("gConvexHull")
    return( gConvexHull(spgeom, id, byid) )
}
RGEOSBoundary = function(spgeom, byid=FALSE, id = NULL) {
    .Deprecated("gBoundary")
    return( gBoundary(spgeom, id, byid) )
}
RGEOSGetCentroid = function(spgeom, byid=FALSE, id = NULL) {
    .Deprecated("gCentroid")
    return( gCentroid(spgeom, id, byid) )
}
RGEOSPointOnSurface = function(spgeom, byid=FALSE, id = NULL) {
    .Deprecated("gPointOnSurface")
    return( gPointOnSurface(spgeom, id, byid) )
}
RGEOSLineMerge = function(spgeom, byid=FALSE, id = NULL) {
    .Deprecated("gLineMerge")
    return( gLineMerge(spgeom, id, byid) )
}
RGEOSUnionCascaded = function(spgeom, id = NULL) {
    .Deprecated("gUnionCascaded")
    return( gUnionCascaded(spgeom, id) )
}




