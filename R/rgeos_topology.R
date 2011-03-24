gSimplify = function(spgeom, tol, topologyPreserve=FALSE) {

	getCutEdges = as.logical(topologyPreserve)
	if (is.na(topologyPreserve))
		stop("Invalid value for topologyPreserve, must be logical")
	
    id = row.names(spgeom)
    return( .Call("rgeos_simplify", .RGEOS_HANDLE, spgeom, tol, id, 
									FALSE, topologyPreserve, PACKAGE="rgeos") )
}

gPolygonize = function( splist, getCutEdges=FALSE) {
	
	if (!is.list(splist))
		splist = list(splist)

	p4slist = lapply(splist,function(x) x@proj4string)
	
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
gUnionCascaded = function(spgeom, id = NULL) {
    
    if (!inherits(spgeom,"SpatialPolygons"))
        stop("Invalid geometry, may only be applied to polygons")

    if (is.null(id))
        id = rep("1",length(row.names(spgeom)))

    return( TopologyFunc(groupID(spgeom,id),unique(na.omit(id)),TRUE,"rgeos_unioncascaded") ) 
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




