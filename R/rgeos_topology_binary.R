RGEOSBinTopoFunc = function(spgeom1, spgeom2, byid, ids=NULL, func) {
    byid = as.logical(byid)
    if (any(is.na(byid)) ) 
        stop("Invalid value for byid, must be logical")
    if (!length(byid) %in% c(1,2))
        stop("Invalid length for byid, must be of length 1 or 2")
    if (length(byid) == 1)
        byid <- rep(byid,2)

    if (missing(spgeom1) | missing(spgeom2))
        stop("spgeom1 and spgeom2 must both be specified")
        
    if(!identical(spgeom1@proj4string,spgeom2@proj4string))
        warning("spgeom1 and spgeom2 have different proj4 strings")
    
    id1 = row.names(spgeom1) 
    id2 = row.names(spgeom2)    
    if(all(byid)) {
        if (is.null(ids)) ids = paste(rep(id1,each=length(id2)),rep(id2,length(id1)))
        idlen = length(id1)*length(id2)
    } else if (byid[1]) {
        if (is.null(ids)) ids = id1
        idlen = length(id1)
    } else if (byid[2]) {
        if (is.null(ids)) ids = id2
        idlen = length(id2)
    } else {
        if (is.null(ids)) ids = "1"
        idlen = 1
    }
    
    if (idlen != length(ids))
        stop(paste("ids vector has incorrect length of",length(ids),"expected length of",idlen))
    
    x <- .Call(func, .RGEOS_HANDLE, spgeom1, spgeom2, byid, ids, PACKAGE="rgeos")
    return(x)
}
gDifference = function(spgeom1, spgeom2, byid=FALSE, id=NULL) {
    return( RGEOSBinTopoFunc(spgeom1, spgeom2, byid, id, "rgeos_difference") )
}
gSymdifference = function(spgeom1, spgeom2, byid=FALSE, id=NULL) {
    return( RGEOSBinTopoFunc(spgeom1, spgeom2, byid, id, "rgeos_symdifference") )
}
gIntersection = function(spgeom1, spgeom2, byid=FALSE, id=NULL) {
    return( RGEOSBinTopoFunc(spgeom1, spgeom2, byid, id, "rgeos_intersection") )
}
gUnion = function(spgeom1, spgeom2, byid=FALSE, id=NULL) {
    return( RGEOSBinTopoFunc(spgeom1, spgeom2, byid, id, "rgeos_union") )
}