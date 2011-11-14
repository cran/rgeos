#include "rgeos.h"



SEXP checkHolesGPC(SEXP env, SEXP A) {

    GEOSGeom GC = GCPPtsGC(env, A);
    return(GC_Contains(env, GC));
}


GEOSGeom GPCptPolygon(SEXP env, SEXP obj) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    GEOSGeom g1 = GPCpt2LinearRing(env, obj);
	GEOSGeom p1 = GEOSGeom_createPolygon_r(GEOShandle, g1, NULL, (unsigned int) 0);
    if (p1 == NULL) {
        //GEOSGeom_destroy_r(GEOShandle, g1);
        error("GPCptPolygon: Polygon not created");
    }

    return(p1);
}

GEOSGeom GPCpt2LinearRing(SEXP env, SEXP obj) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    GEOSCoordSeq s = GPCpt2CoordSeq(env, obj);
    GEOSGeom gl = GEOSGeom_createLinearRing_r(GEOShandle, s);
    if (gl == NULL) {
        //GEOSGeom_destroy_r(GEOShandle, gl);
        error("GPCpt2LinearRing: linearRing not created");
    }


    if (GEOSisValid_r(GEOShandle, gl)) {
		int norm = GEOSNormalize_r(GEOShandle, gl);
		if (norm == -1)
			warning("GPCpt2LinearRing: normalization failure");
    } else {
        warning("GPCpt2LinearRing: validity failure");
    }

    return(gl);
}

GEOSCoordSeq GPCpt2CoordSeq(SEXP env, SEXP obj) {

    int n = length(VECTOR_ELT(obj, 0));
    int m = 2;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    GEOSCoordSeq s = GEOSCoordSeq_create_r(GEOShandle, (n+1), m);

    for(int i=0; i<n; i++) {
		double x = NUMERIC_POINTER(VECTOR_ELT(obj, 0))[i];
		double y = NUMERIC_POINTER(VECTOR_ELT(obj, 1))[i];

		if (GEOSCoordSeq_setX_r(GEOShandle, s, i, x) == 0 || 
			GEOSCoordSeq_setY_r(GEOShandle, s, i, y) == 0) {
            
			//GEOSCoordSeq_destroy_r(GEOShandle, s);
            error("GPCpt2CoordSeq: X or Y not set for %d", i);
        }
    }

	double x = NUMERIC_POINTER(VECTOR_ELT(obj, 0))[0];
	double y = NUMERIC_POINTER(VECTOR_ELT(obj, 1))[0];

	if (GEOSCoordSeq_setX_r(GEOShandle, s, n, x) == 0 || 
		GEOSCoordSeq_setY_r(GEOShandle, s, n, y) == 0) {
        
		//GEOSCoordSeq_destroy_r(GEOShandle, s);
        error("GPCpt2CoordSeq: X or Y not set for %d", n);
    }

    return(s);
}


GEOSGeom GCPPtsGC(SEXP env, SEXP pls) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    int npls = length(pls);
    int nErings;
	int pc=0;
	SEXP comm;
	PROTECT(comm = SP_PREFIX(comment2comm)(pls)); pc++;

	GEOSGeom *geoms;
    if (comm == R_NilValue) {
        geoms = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));

        for (int i=0; i<npls; i++) {
            geoms[i] = GPCptPolygon(env, VECTOR_ELT(pls, i));
        }
    } else {
        nErings = length(comm);
        geoms = (GEOSGeom *) R_alloc((size_t) nErings, sizeof(GEOSGeom));
        
		for (int i=0; i<nErings; i++) {
            geoms[i] = GPCpt_i_Polygon(env, pls, VECTOR_ELT(comm, i));
        }
    }

	GEOSGeom GC = (npls == 1) ? geoms[0]
		: GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOLYGON, geoms, npls);

    if (comm == R_NilValue) {
        for (int i=0; i<npls; i++) GEOSGeom_destroy_r(GEOShandle, geoms[i]);
    } else {
        for (int i=0; i<nErings; i++) GEOSGeom_destroy_r(GEOShandle, geoms[i]);
    }
	if (GC == NULL)
        error("GCPPtsGC: collection not created");

    UNPROTECT(pc);
    return(GC);

}

GEOSGeom GPCpt_i_Polygon(SEXP env, SEXP pls, SEXP vec) {

    GEOSGeom res, pol, hole;
    GEOSGeom *holes;

    int n = length(vec);
    int i, j;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    i = INTEGER_POINTER(vec)[0]-R_OFFSET;

    pol = GPCpt2LinearRing(env, VECTOR_ELT(pls, i));
    if (n == 1) {
        if ((res = GEOSGeom_createPolygon_r(GEOShandle, pol, NULL,
            (unsigned int) 0)) == NULL) {
            //GEOSGeom_destroy_r(GEOShandle, pol);
            error("GPCpt_i_Polygon: Polygon not created");
        }
    } else {
        holes = (GEOSGeom *) R_alloc((size_t) (n-1), sizeof(GEOSGeom));
        for (j=1; j<n; j++) {
            i = INTEGER_POINTER(vec)[j]-R_OFFSET;
            hole = GPCpt2LinearRing(env, VECTOR_ELT(pls, i));
            holes[(j-1)] = hole;
        }
        if ((res = GEOSGeom_createPolygon_r(GEOShandle, pol, holes,
            (unsigned int) (n-1))) == NULL) {
            for (j=0; j<(n-1); j++) GEOSGeom_destroy_r(GEOShandle, holes[j]);
            //GEOSGeom_destroy_r(GEOShandle, pol);
            error("GPCpt_i_Polygon: Polygon not created");
        }
    }
    for (j=0; j<(n-1); j++) GEOSGeom_destroy_r(GEOShandle, holes[j]);
    //GEOSGeom_destroy_r(GEOShandle, pol);
    return(res);
}

SEXP GCGCPPts(SEXP env, GEOSGeom Geom) {
    SEXP res, comment;
    int empty, pc=0;
    int n, nn, nr, i, j, k, jj;
    int *comm;
    GEOSGeom GC, lr;
    char *buf;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    empty = (int) GEOSisEmpty_r(GEOShandle, Geom);
    if (empty == 2) error("GCGCPPts: empty error");
    if (empty == 1) {
        PROTECT(res = NEW_LIST(0)); pc++;
    } else if (GEOSGeomTypeId_r(GEOShandle, Geom) == GEOS_POLYGON) {
        n = GEOSGetNumInteriorRings_r(GEOShandle, Geom) + 1;
        PROTECT(res = NEW_LIST(n)); pc++;

        if ((lr = (GEOSGeometry *) GEOSGetExteriorRing_r(GEOShandle, Geom))
            == NULL) error("GCGCPPts: exterior ring failure");
        comm = (int *) R_alloc((size_t) n, sizeof(int));

        SET_VECTOR_ELT(res, 0, rgeos_LinearRingGCPPts(env, lr, FALSE));

        comm[0] = 0;

        for (i=1; i<n; i++) {
            if ((lr = (GEOSGeometry *) GEOSGetInteriorRingN_r(GEOShandle,
                 Geom, (int) (i-1))) == NULL)
                    error("GCGCPPts: interior ring failure");
            comm[i] = 1;
            SET_VECTOR_ELT(res, i, rgeos_LinearRingGCPPts(env, lr, TRUE));
        }

    } else if (GEOSGeomTypeId_r(GEOShandle, Geom) == GEOS_MULTIPOLYGON) {
        nn = GEOSGetNumGeometries_r(GEOShandle, Geom);

        for (i=0, n=0; i<nn; i++) {
            n += GEOSGetNumInteriorRings_r(GEOShandle, (GEOSGeometry *) 
                GEOSGetGeometryN_r(GEOShandle, Geom, i)) + 1;
        }

        PROTECT(res = NEW_LIST(n)); pc++;
        comm = (int *) R_alloc((size_t) n, sizeof(int));

        for (i=0, j=0; i<nn; i++) {

            GC = (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, Geom, i);

            if ((lr = (GEOSGeometry *) GEOSGetExteriorRing_r(GEOShandle, GC))
                == NULL) error("GCGCPPts: exterior ring failure");

            SET_VECTOR_ELT(res, j, rgeos_LinearRingGCPPts(env, lr, FALSE));

            comm[j] = 0;
            jj = j;
            j++;
            
            nr = GEOSGetNumInteriorRings_r(GEOShandle, GC);
            for (k=0; k<nr; k++) {
                if ((lr = (GEOSGeometry *) GEOSGetInteriorRingN_r(GEOShandle,
                     GC, (int) i)) == NULL)
                     error("GCGCPPts: interior ring failure");
                comm[j] = jj;
                SET_VECTOR_ELT(res, j, rgeos_LinearRingGCPPts(env, lr, TRUE));
                j++;
            }
        }
    }

    int nc;

    nc = ceil(log10(n)+1)+1;
    buf = (char *) R_alloc((size_t) (n*nc)+1, sizeof(char));
    SP_PREFIX(comm2comment)(buf, (n*nc)+1, comm, n);
    PROTECT(comment = NEW_CHARACTER(1)); pc++;
    SET_STRING_ELT(comment, 0, mkChar((const char*) buf));

    setAttrib(res, install("comment"), comment);

    UNPROTECT(pc);
    return(res);

}

SEXP rgeos_LinearRingGCPPts(SEXP env, GEOSGeom lr, int hole) {
    GEOSCoordSeq s;
    unsigned int n;
    SEXP res, nms;
    double val;
    int i, pc=0;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    if ((s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq_r(GEOShandle, lr))
        == NULL) error("rgeos_LinearRingGCPPts: CoordSeq failure");

    GEOSCoordSeq_getSize_r(GEOShandle, s, &n);

    PROTECT(res = NEW_LIST(3)); pc++;
    SET_VECTOR_ELT(res, 0, NEW_NUMERIC(n));
    SET_VECTOR_ELT(res, 1, NEW_NUMERIC(n));
    SET_VECTOR_ELT(res, 2, NEW_LOGICAL(1));
    PROTECT(nms = NEW_CHARACTER(3)); pc++;
    SET_STRING_ELT(nms, 0, COPY_TO_USER_STRING("x"));
    SET_STRING_ELT(nms, 1, COPY_TO_USER_STRING("y"));
    SET_STRING_ELT(nms, 2, COPY_TO_USER_STRING("hole"));
    setAttrib(res, R_NamesSymbol, nms);

    LOGICAL_POINTER(VECTOR_ELT(res, 2))[0] = hole;

    for (i=0; i<n; i++) {
        GEOSCoordSeq_getX_r(GEOShandle, s, i, &val);
        NUMERIC_POINTER(VECTOR_ELT(res, 0))[i] = val;
        GEOSCoordSeq_getY_r(GEOShandle, s, i, &val);
        NUMERIC_POINTER(VECTOR_ELT(res, 1))[i] = val;
    }
    
    //GEOSCoordSeq_destroy_r(GEOShandle,s); 
    
    UNPROTECT(pc);
    return(res);
}


