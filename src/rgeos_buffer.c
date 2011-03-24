#include "rgeos.h"

SEXP rgeos_buffer(SEXP env, SEXP obj, SEXP byid, SEXP id, SEXP width, SEXP quadsegs, 
                  SEXP capStyle, SEXP joinStyle, SEXP mitreLimit) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    GEOSGeom geom = rgeos_convert_R2geos(env, obj);
    SEXP p4s = GET_SLOT(obj, install("proj4string"));
    
    int n;
    if (LOGICAL_POINTER(byid)[0])
        n = GEOSGetNumGeometries_r(GEOShandle, geom);
    else
        n = 1;
    
    GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) n, sizeof(GEOSGeom));
    
    GEOSGeom curgeom = geom;
    for(int i=0; i<n; i++) {
        if ( n > 1) {
            curgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i);
            if (curgeom == NULL) error("rgeos_writeWKT: unable to get subgeometries");
        }
        
        geoms[i] = GEOSBufferWithStyle_r(GEOShandle, curgeom, 
                                         NUMERIC_POINTER(width)[0], 
                                         INTEGER_POINTER(quadsegs)[0], 
                                         INTEGER_POINTER(capStyle)[0], 
                                         INTEGER_POINTER(joinStyle)[0],  
                                         NUMERIC_POINTER(mitreLimit)[0]);
    }
    
    GEOSGeom res;
    if (n == 1) {
        res = geoms[0];
    } else {
        res = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, n);
    }


    SEXP ans = rgeos_convert_geos2R(env, res, p4s, id);

    GEOSGeom_destroy_r(GEOShandle, geom);
    return(ans);
}

