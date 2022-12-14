#include "rgeos.h"

SEXP rgeos_buffer(SEXP env, SEXP obj, SEXP byid, SEXP id, SEXP width,
    SEXP quadsegs, SEXP capStyle, SEXP joinStyle, SEXP mitreLimit) {
    int i, pc=0;
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    GEOSGeometry* geom = rgeos_convert_R2geos(env, obj);
    SEXP p4s = GET_SLOT(obj, install("proj4string"));
    SEXP n_id;
    
//Rprintf("geom is %s; is GC: %d\n", GEOSGeomType_r(GEOShandle, geom), GEOSGeomType_r(GEOShandle, geom) == "GEOS_GEOMETRYCOLLECTION");
    int n;
    if (LOGICAL_POINTER(byid)[0]) {
        n = GEOSGetNumGeometries_r(GEOShandle, geom);
//Rprintf("n %d, length(id) %d\n", n, length(id));
// sanity check 151104 RSB
        if (n > length(id)) {
            PROTECT(n_id = NEW_CHARACTER(n)); pc++;
            char str[15];
            for (i=0; i < n; i++) {
                snprintf(str, sizeof(str), "%d", i+R_OFFSET);
//Rprintf("i %d, str %s\n", i, str);
                SET_STRING_ELT(n_id, i, COPY_TO_USER_STRING(str));
            }
            warning("rgeos_buffer: geometry count/id count mismatch - id changed");
        } else {
            PROTECT(n_id = NEW_CHARACTER(length(id))); pc++;
            for (i=0; i < length(id); i++)
                SET_STRING_ELT(n_id, i, STRING_ELT(id, i));
        }
    } else {
        n = 1;
        PROTECT(n_id = NEW_CHARACTER(length(id))); pc++;
        for (i=0; i < length(id); i++)
            SET_STRING_ELT(n_id, i, STRING_ELT(id, i));
    }
    
    GEOSGeometry** geoms = (GEOSGeometry**) R_alloc((size_t) n, sizeof(GEOSGeometry*));
    SEXP newids;
    PROTECT(newids = NEW_CHARACTER(n)); pc++;
    
    GEOSGeometry* curgeom = geom;
    GEOSGeometry* thisgeom;
    int k = 0;
    for(i=0, k=0; i<n; i++) {
//Rprintf("i %d, k %d, n %d\n", i, k, n);
        if ( n > 1) {
            curgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i);
            if (curgeom == NULL) error("rgeos_buffer: unable to get subgeometries");
        }
//Rprintf("i %d k %d curgeom is %s\n", i, k, GEOSGeomType_r(GEOShandle, curgeom));
        
        thisgeom = GEOSBufferWithStyle_r(GEOShandle, curgeom,//VG FIXME 
                                         NUMERIC_POINTER(width)[i], 
                                         INTEGER_POINTER(quadsegs)[0], 
                                         INTEGER_POINTER(capStyle)[0], 
                                         INTEGER_POINTER(joinStyle)[0],  
                                         NUMERIC_POINTER(mitreLimit)[0]);
// modified 131004 RSB 
// https://stat.ethz.ch/pipermail/r-sig-geo/2013-October/019470.html
        if (!GEOSisEmpty_r(GEOShandle, thisgeom)) {
            geoms[k] = thisgeom;
//Rprintf("i %d k %d thisgeom is %s\n", i, k, GEOSGeomType_r(GEOShandle, thisgeom));
//Rprintf("n_id %s\n", CHAR(STRING_ELT(n_id, i)));
            SET_STRING_ELT(newids, k, STRING_ELT(n_id, i));
            k++;
        }

    }

    GEOSGeom_destroy_r(GEOShandle, geom);

    if (k == 0) {
        UNPROTECT(pc);
        return(R_NilValue);
    }

    GEOSGeometry* res;
    if (k == 1)
        res = geoms[0];
    else
        res = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, (unsigned int) k);
//Rprintf("res is %s\n", GEOSGeomType_r(GEOShandle, res));

    SEXP ans;
    PROTECT(ans = rgeos_convert_geos2R(env, res, p4s, newids)); pc++; // releases res
    UNPROTECT(pc);
    return(ans);
}
