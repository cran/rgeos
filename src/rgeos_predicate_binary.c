#include "rgeos.h"
// symmetric intersects_prepared 
// not symmetric contains_prepared containsproperly_prepared covers_prepared

SEXP rgeos_intersects_prepared(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc_prepared(env,spgeom1,spgeom2,byid, &GEOSPreparedIntersects_r, TRUE) );
}
SEXP rgeos_contains_prepared(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc_prepared(env,spgeom1,spgeom2,byid, &GEOSPreparedContains_r, FALSE) );
}
SEXP rgeos_containsproperly_prepared(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc_prepared(env,spgeom1,spgeom2,byid, &GEOSPreparedContainsProperly_r, FALSE) );
}
SEXP rgeos_covers_prepared(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc_prepared(env,spgeom1,spgeom2,byid, &GEOSPreparedCovers_r, FALSE) );
}

SEXP rgeos_binpredfunc_prepared(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, p_binpredfunc_prepared binpredfunc_prepared, int canSym) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);  
    SEXP returnDense = findVarInFrame(env, install("returnDense"));
    int retDen = LOGICAL_POINTER(returnDense)[0];
//    SEXP STRsubset = findVarInFrame(env, install("STRsubset"));
//    int STRsub = LOGICAL_POINTER(STRsubset)[0];
    int *listvec=NULL, vecn=0;
  
    
    GEOSGeom geom1 = rgeos_convert_R2geos(env, spgeom1);
    int type1 = GEOSGeomTypeId_r(GEOShandle, geom1);
    
    GEOSGeom geom2 = (spgeom2 == R_NilValue) ? geom1 : rgeos_convert_R2geos(env, spgeom2);
    int type2 = GEOSGeomTypeId_r(GEOShandle, geom2);
    
    int S1isS2 = (spgeom2 == R_NilValue);
    
    int m = (LOGICAL_POINTER(byid)[0] && type1 == GEOS_GEOMETRYCOLLECTION) ? 
                GEOSGetNumGeometries_r(GEOShandle, geom1) : 1;
    int n = (LOGICAL_POINTER(byid)[1] && type2 == GEOS_GEOMETRYCOLLECTION) ?
                GEOSGetNumGeometries_r(GEOShandle, geom2) : 1;
    
    if (m == -1) error("rgeos_binpredfunc_prepared: invalid number of subgeometries in geometry 1");
    if (n == -1) error("rgeos_binpredfunc_prepared: invalid number of subgeometries in geometry 2");
    
    int pc = 0;
    SEXP ans;
    if (retDen) {
        if (((double) n * (double) m) >= INT_MAX)
            error("rgeos_binpredfunc_prepared: maximum returned dense matrix size exceeded");
        PROTECT(ans = NEW_LOGICAL(m*n)); pc++;
    } else {
        PROTECT(ans = NEW_LIST(m)); pc++;
        listvec = (int *) R_alloc((size_t) n, sizeof(int));
        vecn = 0;
    }
    
    for(int i=0; i<m; i++) {
        
        const GEOSGeometry *curgeom1 = (m > 1) ? GEOSGetGeometryN_r(GEOShandle, geom1, i) : geom1;
        if (curgeom1 == NULL) 
            error("rgeos_binpredfunc_prepared: unable to get subgeometries from geometry 1");
        
        const GEOSPreparedGeometry *prepgeom = GEOSPrepare_r(GEOShandle, curgeom1);

        for(int j=0; j<n; j++) {
            if(S1isS2 && j > i && canSym && retDen)
                break;
            
            const GEOSGeometry *curgeom2 = (n > 1) ? GEOSGetGeometryN_r(GEOShandle, geom2, j) : geom2;
            if (curgeom2 == NULL) 
                error("rgeos_binpredfunc_prepared: unable to get subgeometries from geometry 2");
            
            int val = (int) binpredfunc_prepared(GEOShandle, prepgeom, curgeom2);
            if (val == 2)
                error("rgeos_binpredfunc_prepared: comparison failed");

            if (retDen) {
                LOGICAL_POINTER(ans)[n*i+j] = val;
                if (S1isS2 && canSym)
                    LOGICAL_POINTER(ans)[n*j+i] = val;
            } else {
                if (val == 1) {
                    listvec[vecn] = j+R_OFFSET;
                    vecn++;
                }
            }
        
        }
        if (!retDen && vecn > 0) {
            SET_VECTOR_ELT(ans, i, NEW_INTEGER(vecn));
            for (int j=0; j<vecn; j++) {
                INTEGER_POINTER(VECTOR_ELT(ans, i))[j] = listvec[j];
            }
            vecn = 0;
        }
        
        GEOSPreparedGeom_destroy_r(GEOShandle, prepgeom);
    }
    
    if ((LOGICAL_POINTER(byid)[0] || LOGICAL_POINTER(byid)[1]) && retDen) {
        SEXP dims;
        PROTECT(dims = NEW_INTEGER(2)); pc++;
        INTEGER_POINTER(dims)[0] = n;
        INTEGER_POINTER(dims)[1] = m;
        setAttrib(ans, R_DimSymbol, dims);
    }
    
    GEOSGeom_destroy_r(GEOShandle, geom1);
    if (!S1isS2)
        GEOSGeom_destroy_r(GEOShandle, geom2);
    
    UNPROTECT(pc);
    return(ans);
}

// symmetric intersects disjoint touches crosses overlaps equals
// not symmetric contains within relate


SEXP rgeos_intersects(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSIntersects_r, TRUE) );
}
SEXP rgeos_contains(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSContains_r, FALSE) );
}
SEXP rgeos_disjoint(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSDisjoint_r, TRUE) );
}
SEXP rgeos_touches(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSTouches_r, TRUE) );
}
SEXP rgeos_crosses(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSCrosses_r, TRUE) );
}
SEXP rgeos_within(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSWithin_r, FALSE) );
}
SEXP rgeos_overlaps(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSOverlaps_r, TRUE) );
}
SEXP rgeos_equals(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSEquals_r, TRUE) );
}
SEXP rgeos_relate(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, (p_binpredfunc) &GEOSRelate_r, FALSE) );
}

SEXP rgeos_binpredfunc(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, p_binpredfunc binpredfunc, int canSym) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    SEXP returnDense = findVarInFrame(env, install("returnDense"));
    int retDen = LOGICAL_POINTER(returnDense)[0];
    int *listvec=NULL, vecn=0;
    int pc = 0;
    
    GEOSGeom geom1 = rgeos_convert_R2geos(env, spgeom1);
    int type1 = GEOSGeomTypeId_r(GEOShandle, geom1);
    
    GEOSGeom geom2 = (spgeom2 == R_NilValue) ? geom1 : rgeos_convert_R2geos(env, spgeom2);
    int type2 = GEOSGeomTypeId_r(GEOShandle, geom2);
    
    int S1isS2 = (spgeom2 == R_NilValue);
    
    int m = (LOGICAL_POINTER(byid)[0] && type1 == GEOS_GEOMETRYCOLLECTION) ? 
                GEOSGetNumGeometries_r(GEOShandle, geom1) : 1;
    int n = (LOGICAL_POINTER(byid)[1] && type2 == GEOS_GEOMETRYCOLLECTION) ?
                GEOSGetNumGeometries_r(GEOShandle, geom2) : 1;
    if (m == -1) error("rgeos_binpredfunc: invalid number of subgeometries in geometry 1");
    if (n == -1) error("rgeos_binpredfunc: invalid number of subgeometries in geometry 2");

    SEXP ans;
    if (!retDen && binpredfunc == (p_binpredfunc) GEOSRelate_r) {
        retDen = TRUE;
        warning("rgeos_binpredfunc: gRelate always returns a dense character matrix");
    }
    if (retDen) {
        if (((double) n * (double) m) >= INT_MAX)
            error("rgeos_binpredfunc: maximum returned dense matrix size exceeded");
        if (binpredfunc == (p_binpredfunc) GEOSRelate_r) {
            PROTECT(ans = NEW_CHARACTER(m*n)); pc++;
        } else {
            PROTECT(ans = NEW_LOGICAL(m*n)); pc++;
        }
    } else {
        PROTECT(ans = NEW_LIST(m)); pc++;
        listvec = (int *) R_alloc((size_t) n, sizeof(int));
        vecn = 0;
    }
    
    for(int i=0; i<m; i++) {
        
        const GEOSGeometry *curgeom1 = (m > 1) ? GEOSGetGeometryN_r(GEOShandle, geom1, i) : geom1;
        if (curgeom1 == NULL) 
            error("rgeos_binpredfunc: unable to get subgeometries from geometry 1");

        for(int j=0; j<n; j++) {
            if(S1isS2 && j > i && canSym && retDen)
                break;
            
            const GEOSGeometry *curgeom2 = (n > 1) ? GEOSGetGeometryN_r(GEOShandle, geom2, j) : geom2;
            if (curgeom2 == NULL) 
                error("rgeos_binpredfunc: unable to get subgeometries from geometry 2");
            
            if (binpredfunc == (p_binpredfunc) GEOSRelate_r) {
                char *buf = (char *) GEOSRelate_r(GEOShandle, curgeom1, curgeom2);
                if (buf == NULL)
                    error("rgeos_isvalidreason: test failed");

                SET_STRING_ELT(ans, n*i+j, COPY_TO_USER_STRING(buf));
                if (S1isS2 && canSym)
                    SET_STRING_ELT(ans, n*j+i, COPY_TO_USER_STRING(buf));

                GEOSFree_r(GEOShandle, buf);
            } else {
                int val = (int) binpredfunc(GEOShandle, curgeom1, curgeom2);
                if (val == 2)
                    error("rgeos_binpredfunc: comparison failed");

                if (retDen) {
                    LOGICAL_POINTER(ans)[n*i+j] = val;
                    if (S1isS2 && canSym)
                        LOGICAL_POINTER(ans)[n*j+i] = val;
                } else {
                    if (val == 1) {
                        listvec[vecn] = j+R_OFFSET;
                        vecn++;
                    }
                }
            }
        }
        if (!retDen && vecn > 0) {
            SET_VECTOR_ELT(ans, i, NEW_INTEGER(vecn));
            for (int j=0; j<vecn; j++) {
                INTEGER_POINTER(VECTOR_ELT(ans, i))[j] = listvec[j];
            }
            vecn = 0;
        }

    }
    
    if ((LOGICAL_POINTER(byid)[0] || LOGICAL_POINTER(byid)[1]) && retDen) {
        SEXP dims;
        PROTECT(dims = NEW_INTEGER(2)); pc++;
        INTEGER_POINTER(dims)[0] = n;
        INTEGER_POINTER(dims)[1] = m;
        setAttrib(ans, R_DimSymbol, dims);
    }
    
    GEOSGeom_destroy_r(GEOShandle, geom1);
    if (!S1isS2)
        GEOSGeom_destroy_r(GEOShandle, geom2);
    
    UNPROTECT(pc);
    return(ans);
}

// symmetric equalsexact
// not symmetric relatepattern

SEXP rgeos_equalsexact(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP tol, SEXP byid) {
    return( rgeos_binpredfunc_opt(env, spgeom1, spgeom2, tol, byid, FALSE, TRUE) );
}

SEXP rgeos_relatepattern(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP pattern, SEXP byid) {
    return( rgeos_binpredfunc_opt(env, spgeom1, spgeom2, pattern, byid, 1, FALSE) );
}

SEXP rgeos_binpredfunc_opt(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP opt, SEXP byid, int relpat, int canSym) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    GEOSGeom geom1 = rgeos_convert_R2geos(env, spgeom1);
    int type1 = GEOSGeomTypeId_r(GEOShandle, geom1);

    GEOSGeom geom2 = (spgeom2 == R_NilValue) ? geom1 : rgeos_convert_R2geos(env, spgeom2);
    int type2 = GEOSGeomTypeId_r(GEOShandle, geom2);
    
    int m = (LOGICAL_POINTER(byid)[0] && type1 == GEOS_GEOMETRYCOLLECTION) ? 
                GEOSGetNumGeometries_r(GEOShandle, geom1) : 1;
    int n = (LOGICAL_POINTER(byid)[1] && type2 == GEOS_GEOMETRYCOLLECTION) ?
                GEOSGetNumGeometries_r(GEOShandle, geom2) : 1;
    
    int S1isS2 = (spgeom2 == R_NilValue);
         
    
    if (m == -1) error("rgeos_equalsexact: invalid number of subgeometries in geometry 1");
    if (n == -1) error("rgeos_equalsexact: invalid number of subgeometries in geometry 2");
    
    SEXP ans;
    int pc=0;
    PROTECT(ans = NEW_LOGICAL(m*n)); pc++;
      
    for(int i=0; i<m; i++) {
        
        const GEOSGeometry *curgeom1 = (m > 1) ? GEOSGetGeometryN_r(GEOShandle, geom1, i) : geom1;
        if (curgeom1 == NULL) 
            error("rgeos_equalsexact: unable to get subgeometries from geometry 1");
        
        for(int j=0; j<n; j++) {
            if(S1isS2 && j > i && canSym)
                break;
            
            const GEOSGeometry *curgeom2 = (n > 1) ? (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom2, j) : geom2;
            if (curgeom2 == NULL) 
                error("rgeos_equalsexact: unable to get subgeometries from geometry 2");
            
            int val;
            if (relpat) {
                char patbuf[BUFSIZ];
                strcpy(patbuf, CHAR( STRING_ELT(opt, 0) ));
                val = (int) GEOSRelatePattern_r(GEOShandle, curgeom1, curgeom2, patbuf);
            } else {
                val = (int) GEOSEqualsExact_r(GEOShandle, curgeom1, curgeom2, NUMERIC_POINTER(opt)[0] );
            }

            if (val == 2)
                error("rgeos_equalsexact: comparison failed");

            LOGICAL_POINTER(ans)[n*i+j] = val;
            if (S1isS2 && canSym)
                LOGICAL_POINTER(ans)[n*j+i] = val;
        }
    }
    
    if (LOGICAL_POINTER(byid)[0] || LOGICAL_POINTER(byid)[1]) {
        SEXP dims;
        PROTECT(dims = NEW_INTEGER(2)); pc++;
        INTEGER_POINTER(dims)[0] = n;
        INTEGER_POINTER(dims)[1] = m;
        setAttrib(ans, R_DimSymbol, dims);
    }
    
    GEOSGeom_destroy_r(GEOShandle, geom1);
    if (!S1isS2)
        GEOSGeom_destroy_r(GEOShandle, geom2);
    
    UNPROTECT(pc);
    return(ans);
}


