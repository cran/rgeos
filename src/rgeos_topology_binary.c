#include "rgeos.h"

SEXP rgeos_difference(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, SEXP ids) {
    return( rgeos_binarytopologyfunc(env, spgeom1, spgeom2, byid, ids, &GEOSDifference_r) );
}
SEXP rgeos_symdifference(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, SEXP ids) {
    return( rgeos_binarytopologyfunc(env, spgeom1, spgeom2, byid, ids, &GEOSSymDifference_r) );
}
SEXP rgeos_intersection(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, SEXP ids) {
    return( rgeos_binarytopologyfunc(env, spgeom1, spgeom2, byid, ids, &GEOSIntersection_r) );
}
SEXP rgeos_union(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, SEXP ids) {
    return( rgeos_binarytopologyfunc(env, spgeom1, spgeom2, byid, ids, &GEOSUnion_r) );
}

SEXP rgeos_binarytopologyfunc(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, SEXP ids, p_bintopofunc bintopofunc) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    // Default to using spgeom1's proj4string
    SEXP p4s = GET_SLOT(spgeom1, install("proj4string"));
    
    GEOSGeom geom1 = rgeos_convert_R2geos(env, spgeom1);
    int type1 = GEOSGeomTypeId_r(GEOShandle, geom1);

    GEOSGeom geom2 = rgeos_convert_R2geos(env, spgeom2);
    int type2 = GEOSGeomTypeId_r(GEOShandle, geom2);

    int m = (LOGICAL_POINTER(byid)[0] && type1 == GEOS_GEOMETRYCOLLECTION) ? 
            GEOSGetNumGeometries_r(GEOShandle, geom1) : 1;

    int n = (LOGICAL_POINTER(byid)[1] && type2 == GEOS_GEOMETRYCOLLECTION) ?
            GEOSGetNumGeometries_r(GEOShandle, geom2) : 1;
    
    if (m == -1) error("rgeos_bintopofunc: invalid number of subgeometries in geometry 1");
    if (n == -1) error("rgeos_bintopofunc: invalid number of subgeometries in geometry 2");

    GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) m*n, sizeof(GEOSGeom));
    
    int k=0;
    for(int i=0; i<m; i++) {

        const GEOSGeometry *curgeom1 = (m > 1) ? GEOSGetGeometryN_r(GEOShandle, geom1, i) : geom1;
        if (curgeom1 == NULL) 
            error("rgeos_bintopofunc: unable to get subgeometries from geometry 1");
        
        for(int j=0; j<n; j++, k++) {
        
            const GEOSGeometry *curgeom2 = (n > 1) ? GEOSGetGeometryN_r(GEOShandle, geom2, j) : geom2;
            if (curgeom2 == NULL) 
                error("rgeos_bintopofunc: unable to get subgeometries from geometry 2");
            
            geoms[k] = bintopofunc(GEOShandle, curgeom1, curgeom2);
            if (geoms[k] == NULL)
                error("rgeos_bintopofunc: topology function failed");
            
            SET_STRING_ELT(ids, k, STRING_ELT(ids, i*n+j));
            
            //Rprintf("%d: %d %d %s\n", k, GEOSisEmpty_r(GEOShandle, geomsk), GEOSGeomTypeId_r(GEOShandle, geomsk), GEOSGeomType_r(GEOShandle, geomsk));
            //if (GEOSisEmpty_r(GEOShandle, geomsk)) continue;
        }
    }
    
    GEOSGeom_destroy_r(GEOShandle, geom1);
    GEOSGeom_destroy_r(GEOShandle, geom2);
    
    if (k == 0)
        return(R_NilValue);
    GEOSGeom res = (k > 1) ? GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, k)
                             : geoms[0];
    
    return( rgeos_convert_geos2R(env, res, p4s, ids) );
}

