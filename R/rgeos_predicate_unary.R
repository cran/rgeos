RGEOSUnaryPredFunc = function(spgeom, byid, func) {
    byid = as.logical(byid)
    if (is.na(byid)) stop("Invalid value for byid, must be logical")
    if (inherits(spgeom, "SpatialPolygons") && get_do_poly_check()) 
        spgeom <- createSPComment(spgeom)

    x <- .Call(func, .RGEOS_HANDLE, spgeom, byid, PACKAGE="rgeos")
    
    if(byid) {
        id <- unique(row.names(spgeom))
        names(x) <- id
    }
    return(x)
}

gIsEmpty = function(spgeom, byid = FALSE) { 
    return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_isempty") )
}
gIsSimple  = function(spgeom, byid = FALSE) { 
    return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_issimple") )
}
gIsRing  = function(spgeom, byid = FALSE) { 
    return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_isring") )
}
gHasZ  = function(spgeom, byid = FALSE) { 
    return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_hasz") )
}
gIsValid  = function(spgeom, byid = FALSE, reason=FALSE) {
	if (reason) 
		return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_isvalidreason") )	    	
	else
		return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_isvalid") )
}

RGEOSisEmpty = function(spgeom, byid = FALSE) { 
    .Deprecated("gIsEmpty")
    return( gIsEmpty(spgeom, byid) )
}
RGEOSisSimple  = function(spgeom, byid = FALSE) { 
    .Deprecated("gIsSimple")
    return( gIsSimple(spgeom, byid) )
}
RGEOSisRing  = function(spgeom, byid = FALSE) { 
    .Deprecated("gIsRing")
    return( gIsRing(spgeom, byid) )
}
RGEOSHasZ  = function(spgeom, byid = FALSE) { 
    .Deprecated("gHasZ")
    return( gHasZ(spgeom, byid) )
}
RGEOSisValid  = function(spgeom, byid = FALSE, reason=FALSE) {
    .Deprecated("gIsValid")
    return( gIsValid(spgeom, byid, reason) )
}
