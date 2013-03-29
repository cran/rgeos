#include "rgeos.h"

SEXP rgeos_geom2bbox(SEXP env, GEOSGeom geom) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    int pc=0;
    
    if (GEOSisEmpty_r(GEOShandle, geom) == 1)
        return(R_NilValue);
    
    GEOSGeom envel = GEOSEnvelope_r(GEOShandle, geom);
    if (envel == NULL)
        return(R_NilValue);
    
    const GEOSGeometry *ext = (GEOSGeomTypeId_r(GEOShandle, envel) != GEOS_POLYGON)
                               ? envel : GEOSGetExteriorRing_r(GEOShandle, envel);
    const GEOSCoordSequence *s = GEOSGeom_getCoordSeq_r(GEOShandle, ext);
    if (s == NULL)
        error("rgeos_geom2bbox: envelope has empty coordinate sequence");
    
    unsigned int i, n;
    GEOSCoordSeq_getSize_r(GEOShandle, s, &n);
    if (n == 0)
        return(R_NilValue);
    
    SEXP bbmat;
    PROTECT(bbmat = rgeos_CoordSeq2crdMat(env, s, 0, FALSE)); pc++;
    
    GEOSGeom_destroy_r(GEOShandle, envel);
    
    SEXP ans;    
    PROTECT(ans = NEW_NUMERIC(4)); pc++;
    NUMERIC_POINTER(ans)[0] = DBL_MAX;
    NUMERIC_POINTER(ans)[1] = DBL_MAX;
    NUMERIC_POINTER(ans)[2] = -DBL_MAX;
    NUMERIC_POINTER(ans)[3] = -DBL_MAX;
    
    for (i=0; i<n; i++) {
        NUMERIC_POINTER(ans)[0] = MIN(NUMERIC_POINTER(ans)[0], NUMERIC_POINTER(bbmat)[i]);
        NUMERIC_POINTER(ans)[1] = MIN(NUMERIC_POINTER(ans)[1], NUMERIC_POINTER(bbmat)[i+n]);
        NUMERIC_POINTER(ans)[2] = MAX(NUMERIC_POINTER(ans)[2], NUMERIC_POINTER(bbmat)[i]);
        NUMERIC_POINTER(ans)[3] = MAX(NUMERIC_POINTER(ans)[3], NUMERIC_POINTER(bbmat)[i+n]);
    }
    
    SEXP dim;
    PROTECT(dim = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dim)[0] = 2;
    INTEGER_POINTER(dim)[1] = 2;
    setAttrib(ans, R_DimSymbol, dim);
    
    SEXP dimnames;
    PROTECT(dimnames = NEW_LIST(2)); pc++;
    SET_VECTOR_ELT(dimnames, 0, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 0), 0, COPY_TO_USER_STRING("x"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 0), 1, COPY_TO_USER_STRING("y"));
    SET_VECTOR_ELT(dimnames, 1, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 0, COPY_TO_USER_STRING("min"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 1, COPY_TO_USER_STRING("max"));
    setAttrib(ans, R_DimNamesSymbol, dimnames);
    
    UNPROTECT(pc);
    return(ans);
}
