#include "rgeos.h"

SEXP rgeos_PolyCreateComment(const SEXP env, const SEXP pls) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    int npls = length(pls);
    
    GEOSGeom *polys = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));
    GEOSGeom *holes = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));
    
    int *polyindex = (int *) R_alloc((size_t) npls, sizeof(int));
    int *holeindex = (int *) R_alloc((size_t) npls, sizeof(int));
    
    int nholes = 0;
    int npolys = 0;
    
    for(int i=0; i<npls; i++) {
        
        SEXP crdMat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
        GEOSGeom geom;
        if (crdMat == R_NilValue) {
            geom = GEOSGeom_createPolygon_r(GEOShandle, NULL, NULL, (unsigned int) 0);
        } else {
            geom = rgeos_crdMat2Polygon(env, crdMat, getAttrib(crdMat, R_DimSymbol));
        }
        
        int hole = LOGICAL_POINTER(GET_SLOT(VECTOR_ELT(pls, i), install("hole")))[0];
        if (hole) {
            holes[nholes] = geom;
            holeindex[nholes] = i;
            nholes++;
        } else {
            polys[npolys] = geom;
            polyindex[npolys] = i;
            npolys++;
        }
    }
    
    if (npolys == 0) error("Polygons object contains only holes and no polygons");
    
    int pc = 0;
    SEXP commentvec;
    PROTECT(commentvec = NEW_INTEGER(npls)); pc++;
    
    for(int i=0; i<npls; i++) {
        INTEGER_POINTER(commentvec)[i] = 0;
    }
    
    if (nholes != 0) {
    
        int *containsindex = (int *) R_alloc((size_t) npolys, sizeof(int));
        for(int i=0; i<nholes; i++) {
            int total = 0;
        
            for(int j=0; j<npolys; j++) {
                // FIXME - Not sure this is the best predicate to use in all cases
                int contains = GEOSContains_r(GEOShandle, polys[j], holes[i]);
            
                if (contains) {
                    containsindex[total] = j;
                    total++;
                }
            }
        
            if (total == 0) {
                error("rgeos_PolyCreateComment: orphaned hole, cannot find containing polygon for hole at index %d", 
                        holeindex[i]+1);
            } else if (total == 1) { // If only one polygon contains the hole we're done
                INTEGER_POINTER(commentvec)[ holeindex[i] ] = polyindex[ containsindex[0] ]+1;
            } else {
                // If multiple polys contain the hole then our best guess is the smallest
                // containing polygon 
            
                int best = 0;
                double bestarea, curarea;
                
                GEOSArea_r(GEOShandle, polys[containsindex[0]], &bestarea);
            
                for(int k=1; k<total; k++) {
                    GEOSArea_r(GEOShandle, polys[containsindex[k]], &curarea);
                    if (curarea < bestarea) {
                        bestarea = curarea;
                        best = k;
                    }
                }
            
                INTEGER_POINTER(commentvec)[ holeindex[i] ] = polyindex[ containsindex[best] ]+1;
            }
        }
    }
	// EJP:
    for(int i=0; i<nholes; i++)
    	GEOSGeom_destroy_r(GEOShandle, holes[i]);
    for(int i=0; i<npolys; i++)
    	GEOSGeom_destroy_r(GEOShandle, polys[i]);
    
    UNPROTECT(pc);
    return(commentvec);
}

SEXP GC_Contains(const SEXP env, const GEOSGeom GC) {

    SEXP ans, dim;
    int pc=0;
    unsigned int i, j, n;
    int contains, ident;

    GEOSGeom Pi, Pj;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    if ( GEOSisValid_r(GEOShandle, GC) ) {
        GEOSGeom_destroy_r(GEOShandle, GC);
        return(R_NilValue);
    }

    n = (unsigned int) GEOSGetNumGeometries_r(GEOShandle, GC);
    PROTECT(ans = NEW_LIST(2)); pc++;
    SET_VECTOR_ELT(ans, 0, NEW_LOGICAL((int) (n*n)));
    SET_VECTOR_ELT(ans, 1, NEW_LOGICAL((int) (n*n)));
    PROTECT(dim = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dim)[0] = (int) n;
    INTEGER_POINTER(dim)[1] = (int) n;
    setAttrib(VECTOR_ELT(ans, 0), R_DimSymbol, dim);
    setAttrib(VECTOR_ELT(ans, 1), R_DimSymbol, dim);

    for (i=0; i<n; i++) {
        if ((Pi = (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, GC, (int) i)) == NULL) {
            GEOSGeom_destroy_r(GEOShandle, GC);
            UNPROTECT(pc);
            return(R_NilValue);
        } // Pi invalid
            for (j=0; j<n; j++) {
                if ((Pj = (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, GC, (int) j)) == NULL) {
                    GEOSGeom_destroy_r(GEOShandle, GC);
                    UNPROTECT(pc);
                    return(R_NilValue);
                } // Pj invalid
                if (i == j) {
                    LOGICAL_POINTER(VECTOR_ELT(ans, 0))[i+(j*n)] = FALSE;
                    LOGICAL_POINTER(VECTOR_ELT(ans, 1))[i+(j*n)] = FALSE;
                } else { // i == j
                    contains = (int) GEOSContains_r(GEOShandle, Pi, Pj);
                    if (contains == 2) {
                        LOGICAL_POINTER(VECTOR_ELT(ans, 0))[i+(j*n)] = NA_LOGICAL;
                        LOGICAL_POINTER(VECTOR_ELT(ans, 1))[i+(j*n)] = NA_LOGICAL;
                    } else { // contains invalid
                        ident = (int) GEOSEquals_r(GEOShandle, Pi, Pj);
                        if (ident == 2) {
                            LOGICAL_POINTER(VECTOR_ELT(ans, 0))[i+(j*n)] = NA_LOGICAL;
                            LOGICAL_POINTER(VECTOR_ELT(ans, 1))[i+(j*n)] = NA_LOGICAL;
                        } else { // ident invalid
                            LOGICAL_POINTER(VECTOR_ELT(ans, 0))[i+(j*n)] = contains;
                            LOGICAL_POINTER(VECTOR_ELT(ans, 1))[i+(j*n)] = ident;
                        } // ident valid
                    } // contains valid
                } // i != j
            } // j

    } // i
    GEOSGeom_destroy_r(GEOShandle, GC);

    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_PolygonsContain(const SEXP env, const SEXP obj) {

    GEOSGeom GC = rgeos_convert_R2geos(env, obj);

    return(GC_Contains(env, GC));
}

