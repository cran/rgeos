#include "rgeos.h"


SEXP rgeos_readWKT(SEXP env, SEXP obj, SEXP p4s, SEXP id) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    GEOSWKTReader *reader = GEOSWKTReader_create_r(GEOShandle);
    GEOSGeom geom = GEOSWKTReader_read_r(GEOShandle,reader, CHAR(STRING_ELT(obj, 0))); //VG FIXME
    GEOSWKTReader_destroy_r(GEOShandle,reader);
    
    if (geom == NULL) error("rgeos_readWKT: unable to read wkt");
    
    SEXP ans = rgeos_convert_geos2R(env, geom, p4s, id); // destroys geom
    
    return(ans);
}


SEXP rgeos_writeWKT(SEXP env, SEXP obj, SEXP byid) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    GEOSGeom geom = rgeos_convert_R2geos(env, obj);
    
    int n = (LOGICAL_POINTER(byid)[0]) ? GEOSGetNumGeometries_r(GEOShandle, geom) : 1;
     
    int pc=0;
    SEXP ans;
    PROTECT(ans = NEW_CHARACTER(n)); pc++;
    
    GEOSWKTWriter *writer = GEOSWKTWriter_create_r(GEOShandle);
    GEOSGeom curgeom = geom;
    for(int i=0; i<n; i++) {
        if ( n > 1) {
            curgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i);
            if (curgeom == NULL) error("rgeos_writeWKT: unable to get subgeometries");
        }
        
        char *buf = GEOSWKTWriter_write_r(GEOShandle, writer, curgeom);
        if (buf == NULL) error("rgeos_writeWKT: unable to write wkt");
        
        SET_STRING_ELT(ans, i, COPY_TO_USER_STRING(buf));
        
        GEOSFree_r(GEOShandle, buf);
    }
    
    GEOSWKTWriter_destroy_r(GEOShandle,writer);
    GEOSGeom_destroy_r(GEOShandle, geom);
    
    UNPROTECT(pc);
    
    return(ans);
}

