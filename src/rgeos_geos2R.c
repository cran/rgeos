#include "rgeos.h"

SEXP rgeos_convert_geos2R(SEXP env, GEOSGeom geom, SEXP p4s, SEXP id) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);

/*    int norm = GEOSNormalize_r(GEOShandle, geom);
    if (norm == -1) {
        GEOSGeom_destroy_r(GEOShandle, geom);
        error("rgeos_convert_geos2R: normalization failed");
    }*/

    int type = GEOSGeomTypeId_r(GEOShandle, geom);
    int ng = GEOSGetNumGeometries_r(GEOShandle, geom);
    if (ng == -1) error("rgeos_convert_geos2R: invalid number of subgeometries"); 
    
    if (type == GEOS_GEOMETRYCOLLECTION && ng==0 && GEOSisEmpty_r(GEOShandle,geom)) {
        GEOSGeom_destroy_r(GEOShandle, geom);
        return(R_NilValue);
    }
    
    ng = ng ? ng : 1; // Empty MULTI type geometries return size 0

    int pc=0;

    SEXP ans=NULL, cls;
    switch(type) { // Determine appropriate conversion for the collection
        case -1:
            error("rgeos_convert_geos2R: unknown geometry type");
            break;
            
        case GEOS_POINT:
        case GEOS_MULTIPOINT:
            PROTECT( ans = rgeos_geospoint2SpatialPoints(env, geom, p4s, id, ng) ); pc++;
            break;
    
        case GEOS_LINEARRING:
            PROTECT( ans = rgeos_geosring2SpatialRings(env, geom, p4s, id, ng)); pc++;
            break;
            
        case GEOS_LINESTRING:
        case GEOS_MULTILINESTRING:
            PROTECT( ans = rgeos_geosline2SpatialLines(env, geom, p4s, id, 1) ); pc++;
            break;
    
        case GEOS_POLYGON:
        case GEOS_MULTIPOLYGON:
            PROTECT( ans = rgeos_geospolygon2SpatialPolygons(env, geom,p4s, id, 1) ); pc++;
            break;
        
        case GEOS_GEOMETRYCOLLECTION:
        {    
            
            int gctypes[] = {0,0,0,0,0,0,0,0};
            int gctypen[] = {0,0,0,0,0,0,0,0};
            int n=0;
            
            int *types = (int *) R_alloc((size_t) ng, sizeof(int));
            for (int i=0; i<ng; i++) {
                const GEOSGeometry *subgeom = GEOSGetGeometryN_r(GEOShandle, geom, i);
                if (subgeom == NULL)
                    error("rgeos_convert_geos2R: unable to retrieve subgeometry");
                
                int ns = GEOSGetNumGeometries_r(GEOShandle, subgeom);
                if (ns == -1) error("rgeos_convert_geos2R: invalid number of geometries in subgeometry");
                ns = ns ? ns : 1;
                n += ns;
                
                types[i] = GEOSGeomTypeId_r(GEOShandle, subgeom);
                if (types[i] == GEOS_GEOMETRYCOLLECTION) {
                    Rprintf("output subgeometry %d, row.name: %s\n", i,
                        CHAR(STRING_ELT(id, i)));
                    for (int ii=0; ii<ns; ii++)
                        Rprintf("subsubgeometry %d: %s\n", ii,//VG FIXME
                            GEOSGeomType_r(GEOShandle,
                            GEOSGetGeometryN_r(GEOShandle, subgeom, ii)));
                    error("Geometry collections may not contain other geometry collections");
                }
                
                gctypes[ types[i] ] += 1; 
                gctypen[ types[i] ] += ns;
            }
            
            int isPoint = gctypes[GEOS_POINT] + gctypes[GEOS_MULTIPOINT];
            int isLine  = gctypes[GEOS_LINESTRING] + gctypes[GEOS_MULTILINESTRING];
            int isPoly  = gctypes[GEOS_POLYGON] + gctypes[GEOS_MULTIPOLYGON];
            int isRing  = gctypes[GEOS_LINEARRING];
            int isGC    = gctypes[GEOS_GEOMETRYCOLLECTION];
            
            if ( isPoint && !isLine && !isPoly && !isRing && !isGC ) {
                PROTECT( ans = rgeos_geospoint2SpatialPoints(env, geom, p4s, id, n) ); pc++;
            } else if ( isLine && !isPoint && !isPoly && !isRing && !isGC ) {
                PROTECT( ans = rgeos_geosline2SpatialLines(env, geom, p4s, id, ng) ); pc++;
            } else if ( isPoly && !isPoint && !isLine && !isRing && !isGC ) {
                PROTECT( ans = rgeos_geospolygon2SpatialPolygons(env, geom, p4s,id, ng) ); pc++;
            } else if ( isRing && !isPoint && !isLine && !isPoly && !isGC ) {
                PROTECT( ans = rgeos_geosring2SpatialRings(env, geom, p4s, id, ng) ); pc++;    
            } else {
                
                //Rprintf("isPoint: %d  isLine: %d  isPoly: %d  isRing: %d  isGC: %d\n",isPoint, isLine, isPoly, isRing, isGC);
                
                int m = MAX(MAX(MAX(isPoint,isLine),isPoly),isRing);
                if (length(id) < m) {
                    char buf[BUFSIZ];

                    PROTECT(id = NEW_CHARACTER(m)); pc++;
                    for (int i=0;i<m;i++) {
                        snprintf(buf, sizeof(buf), "%d", i);
                        SET_STRING_ELT(id, i, COPY_TO_USER_STRING(buf));
                    }
                }
                
                GEOSGeom *GCS[4];
                GCS[0] = (GEOSGeom *) R_alloc((size_t) isPoint, sizeof(GEOSGeom));
                GCS[1] = (GEOSGeom *) R_alloc((size_t) isLine,  sizeof(GEOSGeom));
                GCS[2] = (GEOSGeom *) R_alloc((size_t) isRing,  sizeof(GEOSGeom));
                GCS[3] = (GEOSGeom *) R_alloc((size_t) isPoly,  sizeof(GEOSGeom));
                
                SEXP ptID, lID, rID, pID;
                PROTECT(ptID = NEW_CHARACTER(isPoint)); pc++;
                PROTECT(lID  = NEW_CHARACTER(isLine)); pc++;
                PROTECT(rID  = NEW_CHARACTER(isRing)); pc++;
                PROTECT(pID  = NEW_CHARACTER(isPoly)); pc++;
                
                int typei[] = {0,0,0,0};
                for (int i=0; i<ng; i++) {
                    const GEOSGeometry *subgeom = GEOSGetGeometryN_r(GEOShandle, geom, i);
                    if (subgeom == NULL)
                        error("rgeos_convert_geos2R: unable to retrieve subgeometry");
                    
                    int j = -1;
                    SEXP cur_id=NULL;
                    
                    if (types[i]==GEOS_POINT || types[i]==GEOS_MULTIPOINT) {
                        j=0;
                        cur_id=ptID;
                    } else if (types[i]==GEOS_LINESTRING || types[i]==GEOS_MULTILINESTRING) {
                        j=1;
                        cur_id=lID;
                    } else if (types[i]==GEOS_LINEARRING) {
                        j=2;
                        cur_id=rID;
                    } else if (types[i]==GEOS_POLYGON || types[i]==GEOS_MULTIPOLYGON) {
                        j=3;
                        cur_id=pID;
                    }
                    
                    if (GCS[j] == NULL)
                        error("rgeos_convert_geos2R: GCS element is NULL (this should never happen).");
                    
                    GCS[j][ typei[j] ] = GEOSGeom_clone_r(GEOShandle, subgeom);
                    
                    SET_STRING_ELT(cur_id, typei[j], STRING_ELT(id,typei[j]));
                    typei[j]++;
                }         
                
                SEXP points = R_NilValue;
                SEXP lines  = R_NilValue;
                SEXP rings  = R_NilValue;
                SEXP polys  = R_NilValue;
                
                if (isPoint) {
                    GEOSGeom ptGC = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, GCS[0], (unsigned int) isPoint);
                    PROTECT( points = rgeos_convert_geos2R(env, ptGC, p4s, ptID) ); pc++;
                }
                if (isLine) {
                    GEOSGeom lGC = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, GCS[1], (unsigned int) isLine);
                    PROTECT( lines = rgeos_convert_geos2R(env, lGC, p4s, lID) ); pc++;
                }
                if (isRing) {
                    GEOSGeom rGC = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, GCS[2], (unsigned int) isRing);
                    PROTECT( rings = rgeos_convert_geos2R(env, rGC, p4s, rID) ); pc++;
                }
                if (isPoly) {
                    GEOSGeom pGC = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, GCS[3], (unsigned int) isPoly);
                    PROTECT( polys = rgeos_convert_geos2R(env, pGC, p4s, pID) ); pc++;
                }
// rchk PROTECT MAKE_CLASS RSB 180602
                PROTECT(cls = MAKE_CLASS("SpatialCollections")); pc++;
                PROTECT(ans = NEW_OBJECT(cls)); pc++;
                SET_SLOT(ans, install("proj4string"), p4s);
                
                SET_SLOT(ans, install("pointobj"), points);
                SET_SLOT(ans, install("lineobj"), lines);
                SET_SLOT(ans, install("ringobj"), rings);
                SET_SLOT(ans, install("polyobj"), polys);
            
                SEXP plotOrder;
                PROTECT(plotOrder = NEW_INTEGER(4)); pc++;
                INTEGER_POINTER(plotOrder)[0] = 4;
                INTEGER_POINTER(plotOrder)[1] = 3;
                INTEGER_POINTER(plotOrder)[2] = 2;
                INTEGER_POINTER(plotOrder)[3] = 1;
                SET_SLOT(ans, install("plotOrder"), plotOrder);
                
                SEXP bbox;
                PROTECT(bbox = rgeos_geom2bbox(env, geom)); pc++;
                SET_SLOT(ans, install("bbox"), bbox);
            }
            
            break;
        }    
        default:
            error("rgeos_convert_geos2R: Unknown geometry type");
    }
    
    GEOSGeom_destroy_r(GEOShandle, geom);
    UNPROTECT(pc);
    return(ans);
}


SEXP rgeos_geospolygon2SpatialPolygons(SEXP env, GEOSGeom geom, SEXP p4s, SEXP IDs, int ng) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    int pc=0;
    int nng = ng;
    SEXP bbox=R_NilValue, comment;
    GEOSGeom bb;

    
    int type = GEOSGeomTypeId_r(GEOShandle, geom);
    int empty = GEOSisEmpty_r(GEOShandle, geom);
    if (ng < 1) 
        error("rgeos_geospolygon2SpatialPolygons: invalid number of geometries");
    
    if (ng > length(IDs))
        error("rgeos_geospolygon2SpatialPolygons: ng > length(IDs)");

    double polyT = NUMERIC_POINTER(findVarInFrame(env,
        install("polyThreshold")))[0];
    int dropSlivers = LOGICAL_POINTER(findVarInFrame(env,
        install("dropSlivers")))[0];
    int warnSlivers = LOGICAL_POINTER(findVarInFrame(env,
        install("warnSlivers")))[0];
    double iarea = 0.0;
    int *keep = (int *) R_alloc((size_t) ng, sizeof(int));
    int ing=0;
    for (int i=0; i<ng; i++) {
        
        GEOSGeom GC = (type == GEOS_GEOMETRYCOLLECTION && !empty) ?
            (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, geom, i) :
            geom;
        
        if (GC == NULL) 
            error("rgeos_geospolygon2SpatialPolygons: unable to get subgeometry");
        keep[i] = TRUE;
        GEOSArea_r(GEOShandle, GC, &iarea);
//Rprintf("%g %g\n", iarea, polyT);
        if (iarea < polyT) {
            keep[i] = FALSE;
            ing++;
            if (warnSlivers) warning("%d: %s object %s area %g", ing, //VG FIXME
                GEOSGeomType_r(GEOShandle, GC), CHAR(STRING_ELT(IDs, i)),
                iarea);
        }
//Rprintf("keep: %d, type: %s, area: %g, ID: %s\n", keep[i],  GEOSGeomType_r(GEOShandle, GC), iarea, CHAR(STRING_ELT(IDs, i)));
    }

    GEOSGeom *bbs;
    if (dropSlivers) {
        nng = ng - ing;
        if (nng == 0) {
            if (warnSlivers)
                warning("No remaining geometries at threshold %g", polyT);
            return(R_NilValue);
        } else if (ng == nng) {
            dropSlivers = !dropSlivers;
        } else {
            bbs = (GEOSGeom *) R_alloc((size_t) nng,
                sizeof(GEOSGeom));
        }
    }
    if (!dropSlivers) {
        PROTECT(bbox = rgeos_geom2bbox(env, geom)); pc++;
    }


    SEXP pls;
    PROTECT(pls = NEW_LIST(nng)); pc++;
    
    double *areas = (double *) R_alloc((size_t) nng, sizeof(double));
    int *po = (int *) R_alloc((size_t) nng, sizeof(int));
    int ii=0;
    for (int i=0; i<ng; i++) {
        if ((dropSlivers && keep[i]) || !dropSlivers) {
        
            GEOSGeom GC = (type == GEOS_GEOMETRYCOLLECTION && !empty) ?
                (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, geom, i) :
                    geom;
        
            if (GC == NULL) 
                error("rgeos_geospolygon2SpatialPolygons: unable to get subgeometry");


            SEXP poly, ID;
            PROTECT( ID = NEW_CHARACTER(1));
            SET_STRING_ELT(ID,0,STRING_ELT(IDs, i));
            PROTECT( poly = rgeos_geospolygon2Polygons(env, GC, ID) );

            if (dropSlivers) {
                if ((bb = GEOSEnvelope_r(GEOShandle,
                    (GEOSGeom) rgeos_Polygons2MP(env, poly))) == NULL) {
                    error("rgeos_geospolygon2SpatialPolygons: envelope [%d] not created", i);
                }
                bbs[ii] = bb;
//Rprintf("bb is %s\n", GEOSGeomType_r(GEOShandle, bb));
            }
        
            areas[ii] = NUMERIC_POINTER(GET_SLOT(poly, install("area")))[0];
            SET_VECTOR_ELT(pls, ii, poly);
        
            po[ii] = ii + R_OFFSET;

            UNPROTECT(2); 
            ii++;
        }
    }

    if (dropSlivers) {
        PROTECT(bbox = rgeos_geom2bbox(env,//VG FIXME
            GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOLYGON,
                bbs, (unsigned int) nng))); pc++;
        
        for (int i=0; i<nng; i++) {
            GEOSGeom_destroy_r(GEOShandle, bbs[i]);
        }
    }
    
    revsort(areas, po, nng);
    
    SEXP plotOrder;
    PROTECT(plotOrder = NEW_INTEGER(nng)); pc++;
    for (int i=0; i<nng; i++) 
        INTEGER_POINTER(plotOrder)[i] = po[i];
    
    SEXP ans, cls;
// rchk PROTECT MAKE_CLASS RSB 180602
    PROTECT(cls = MAKE_CLASS("SpatialPolygons")); pc++;
    PROTECT(ans = NEW_OBJECT(cls)); pc++;
    SET_SLOT(ans, install("polygons"), pls);
    SET_SLOT(ans, install("proj4string"), p4s);
    SET_SLOT(ans, install("plotOrder"), plotOrder);
    SET_SLOT(ans, install("bbox"), bbox);
// RSB 120417 add top-level comment that all member Polygons
// objects have comment set
    PROTECT(comment = NEW_CHARACTER(1)); pc++;
    SET_STRING_ELT(comment, 0, mkChar("TRUE"));
    setAttrib(ans, install("comment"), comment);

    UNPROTECT(pc);
    return(ans);
}



SEXP rgeos_geospolygon2Polygons(SEXP env, GEOSGeom geom, SEXP ID) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    int pc=0;
    double polyT = NUMERIC_POINTER(findVarInFrame(env,
        install("polyThreshold")))[0];
    double totalarea = 0.0;
    int dropSlivers = LOGICAL_POINTER(findVarInFrame(env,
        install("dropSlivers")))[0];
    int warnSlivers = LOGICAL_POINTER(findVarInFrame(env,
        install("warnSlivers")))[0];

//    GEOSArea_r(GEOShandle, geom, &totalarea);
//Rprintf("%g %g\n", totalarea, polyT);
//    if (totalarea < polyT)
//        warning("Polygons object %s area %g", CHAR(STRING_ELT(ID, 0)),
//            totalarea);

    
    int type = GEOSGeomTypeId_r(GEOShandle, geom);    
    int empty = GEOSisEmpty_r(GEOShandle, geom);
    int ngeom = GEOSGetNumGeometries_r(GEOShandle, geom);
    ngeom = ngeom ? ngeom : 1;
    
    int npoly = 0;
    
    for (int i=0; i<ngeom; i++) {
        GEOSGeom GC = (type == GEOS_MULTIPOLYGON && !empty) ?
                        (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, geom, i) :
                        geom;
        int GCempty = GEOSisEmpty_r(GEOShandle, GC);
        int GCpolys = (GCempty) ? 1 :
                        GEOSGetNumInteriorRings_r(GEOShandle, GC) + 1;


        npoly += GCpolys;
    }

    int *keep = (int *) R_alloc((size_t) npoly, sizeof(int));
    int kk1=npoly;
    if (polyT > 0.0) {
        int kk=0;
        kk1=0;
        double iiarea, maxiiarea=0.0;
        int n_maxarea=-1;
        for (int ii=0; ii<ngeom; ii++) {
            keep[ii] = TRUE;
            GEOSGeom GC = (type == GEOS_MULTIPOLYGON && !empty) ?
                (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, geom, ii) :
                geom;
        
            if (GEOSisEmpty_r(GEOShandle, GC)) 
                error("rgeos_geospolygon2Polygons: empty Polygons object");

            GEOSGeom lr = (GEOSGeometry *) GEOSGetExteriorRing_r(GEOShandle,
                GC);
            if (lr == NULL)
                error("rgeos_geospolygon2Polygons: exterior ring failure");
            GEOSArea_r(GEOShandle, GEOSGeom_createPolygon_r(GEOShandle, lr, NULL, (unsigned int) 0), &iiarea);//VG FIXME
            if (iiarea < polyT) {
                keep[kk] = FALSE;
                if (iiarea > maxiiarea) n_maxarea = kk;
                if (warnSlivers) 
                    warning("Exterior ring %d of object %s area %g", ii,
                    CHAR(STRING_ELT(ID, 0)), iiarea);
            } else {
                kk1++;
            }
            kk++;

            int nirs = GEOSGetNumInteriorRings_r(GEOShandle, GC);
            for (int j=0; j<nirs; j++) {
            
                lr = (GEOSGeometry *) GEOSGetInteriorRingN_r(GEOShandle, GC, j);
                if (lr == NULL)
                    error("rgeos_geospolygon2Polygons: interior ring failure");
            
                GEOSArea_r(GEOShandle, GEOSGeom_createPolygon_r(GEOShandle, lr, NULL, (unsigned int) 0), &iiarea);
                if (iiarea < polyT) {
                    keep[kk] = FALSE;
                    if (iiarea > maxiiarea) n_maxarea = kk;
                    if (warnSlivers) 
                        warning("Interior ring %d of Polygon %d of object %s area %g", j, ii, CHAR(STRING_ELT(ID, 0)), iiarea);
                } else {
                    kk1++;
                }
                kk++;
            }
        }
        if (kk1 == 0 && dropSlivers) {
            if (n_maxarea < 0 || n_maxarea >= npoly)
                error("n_maxarea %d out of bounds 0:%d", n_maxarea, npoly);
            keep[n_maxarea] = TRUE;
            kk1++;
        }
    }

    if (polyT > 0.0 && dropSlivers) {
        npoly = kk1;
    }
    
    SEXP polys;
    PROTECT(polys = NEW_LIST(npoly)); pc++;
    int *comm = (int *) R_alloc((size_t) npoly, sizeof(int));
    int *po = (int *) R_alloc((size_t) npoly, sizeof(int));
    double *areas = (double *) R_alloc((size_t) npoly, sizeof(double));
    
    totalarea = 0.0;
    int k = 0;
    int ownerk=0;
    if (polyT > 0.0 && dropSlivers) {
        int kk = 0;
        for (int i=0; i<ngeom; i++) {
            GEOSGeom GC = (type == GEOS_MULTIPOLYGON && !empty) ?
                (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, geom, i) :
                 geom;
        
            if (GEOSisEmpty_r(GEOShandle, GC)) {
            
                error("rgeos_geospolygon2Polygons: empty Polygons object");
            
            } else {
        
                GEOSGeom lr = (GEOSGeometry *) GEOSGetExteriorRing_r(GEOShandle, GC);
                if (lr == NULL)
                    error("rgeos_geospolygon2Polygons: exterior ring failure");

                if (keep[k]) {
                    SET_VECTOR_ELT(polys, kk, rgeos_geosring2Polygon(env,
                        lr, FALSE));
                    comm[kk] = 0;
        
                    areas[kk] = NUMERIC_POINTER( GET_SLOT(VECTOR_ELT(polys,
                        kk), install("area")) )[0];
                    totalarea += areas[kk];
                    po[kk] = kk + R_OFFSET;
        
                    ownerk = kk + R_OFFSET;
                    kk++;
                }
        
                k++;
        
                int nirs = GEOSGetNumInteriorRings_r(GEOShandle, GC);
                for (int j=0; j<nirs; j++) {
                    lr = (GEOSGeometry *) GEOSGetInteriorRingN_r(GEOShandle, GC, j);
                    if (lr == NULL)
                        error("rgeos_geospolygon2Polygons: interior ring failure");
                    if (keep[k]) {
                        SET_VECTOR_ELT(polys, kk, rgeos_geosring2Polygon(env, lr, TRUE));
                        comm[kk] = ownerk;
            
                        areas[kk] = NUMERIC_POINTER( GET_SLOT(VECTOR_ELT(polys,
                            kk), install("area")) )[0];
                        po[kk] = kk + R_OFFSET;
                        kk++;
                    }
                    k++;
                }
            }
        }
    } else {
        for (int i=0; i<ngeom; i++) {
            GEOSGeom GC = (type == GEOS_MULTIPOLYGON && !empty) ?
                (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, geom, i) :
                 geom;
        
            if (GEOSisEmpty_r(GEOShandle, GC)) {
            
                error("rgeos_geospolygon2Polygons: empty Polygons object");
            
            } else {
        
                GEOSGeom lr = (GEOSGeometry *) GEOSGetExteriorRing_r(GEOShandle, GC);
                if (lr == NULL)
                    error("rgeos_geospolygon2Polygons: exterior ring failure");
                SET_VECTOR_ELT(polys, k, rgeos_geosring2Polygon(env,
                    lr, FALSE));
                comm[k] = 0;
        
                areas[k] = NUMERIC_POINTER( GET_SLOT(VECTOR_ELT(polys, k),
                    install("area")) )[0];
                totalarea += areas[k];
                po[k] = k + R_OFFSET;
        
                ownerk = k + R_OFFSET;
      
                k++;
        
                int nirs = GEOSGetNumInteriorRings_r(GEOShandle, GC);
                for (int j=0; j<nirs; j++) {
            
                    lr = (GEOSGeometry *) GEOSGetInteriorRingN_r(GEOShandle, GC, j);
                    if (lr == NULL)
                        error("rgeos_geospolygon2Polygons: interior ring failure");
            
                    SET_VECTOR_ELT(polys, k, rgeos_geosring2Polygon(env, lr, TRUE));
                    comm[k] = ownerk;
            
                    areas[k] = NUMERIC_POINTER( GET_SLOT(VECTOR_ELT(polys,
                        k), install("area")) )[0];
                    po[k] = k + R_OFFSET;
            
                    k++;
                    
                }
            }
        }
    } 
    
    SEXP plotOrder;
    PROTECT(plotOrder = NEW_INTEGER(npoly)); pc++;
    revsort(areas, po, npoly);
    for (int i=0; i<npoly; i++) 
        INTEGER_POINTER(plotOrder)[i] = po[i];
    
    SEXP labpt = GET_SLOT(VECTOR_ELT(polys,po[0]-1), install("labpt"));
    
    SEXP area;
    PROTECT(area = NEW_NUMERIC(1)); pc++;
    NUMERIC_POINTER(area)[0] = totalarea;
    
    SEXP comment;
    PROTECT(comment = NEW_CHARACTER(1)); pc++;
    char *buf;
    int nc;

    nc = (int) (ceil(log10(npoly)+1.0))+1;
    buf = (char *) R_alloc((size_t) (npoly*nc)+1, sizeof(char));
    SP_PREFIX(comm2comment)(buf, (npoly*nc)+1, comm, npoly);
    SET_STRING_ELT(comment, 0, mkChar((const char*) buf));

    SEXP ans, cls;
// rchk PROTECT MAKE_CLASS RSB 180602
    PROTECT(cls = MAKE_CLASS("Polygons")); pc++;
    PROTECT(ans = NEW_OBJECT(cls)); pc++;    
    SET_SLOT(ans, install("Polygons"), polys);
    SET_SLOT(ans, install("plotOrder"), plotOrder);
    SET_SLOT(ans, install("labpt"), labpt);
    SET_SLOT(ans, install("ID"), ID);
    SET_SLOT(ans, install("area"), area);
    setAttrib(ans, install("comment"), comment);

    UNPROTECT(pc);
    return(ans);
}


SEXP rgeos_geosring2Polygon(SEXP env, GEOSGeom lr, int hole) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    int pc=0;
    
    GEOSCoordSeq s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq_r(GEOShandle, lr);
    if (s == NULL) 
        error("rgeos_geosring2Polygon: CoordSeq failure");
    
    unsigned int n;
    if (GEOSCoordSeq_getSize_r(GEOShandle, s, &n) == 0)
        error("rgeos_geosring2Polygon: CoordSeq failure");
    
    // Get coordinates
    SEXP crd;
    PROTECT(crd = rgeos_crdMatFixDir(PROTECT(rgeos_CoordSeq2crdMat(env, s, FALSE, hole)), hole)); pc += 2;
    
    // Calculate area
    GEOSGeom p = GEOSGeom_createPolygon_r(GEOShandle,GEOSGeom_clone_r(GEOShandle,lr),NULL,0);
    if (p == NULL) 
        error("rgeos_geosring2Polygon: unable to create polygon");
    
    SEXP area;
    PROTECT(area = NEW_NUMERIC(1)); pc++;
    NUMERIC_POINTER(area)[0] = 0.0;
    if (!GEOSArea_r(GEOShandle, p, NUMERIC_POINTER(area)))
        error("rgeos_geosring2Polygon: area calculation failure");
    
    
    // Calculate label position
    SEXP labpt;
    PROTECT(labpt = NEW_NUMERIC(2)); pc++;
    
    GEOSGeom centroid = GEOSGetCentroid_r(GEOShandle, p);
    double xc, yc;
    rgeos_Pt2xy(env, centroid, &xc, &yc);
    
    if (!R_FINITE(xc) || !R_FINITE(yc)) {
        xc = 0.0;
        yc = 0.0;
        for(int i=0; i != n; i++) {
            xc += NUMERIC_POINTER(crd)[i];
            yc += NUMERIC_POINTER(crd)[(int) (n) +i];
        }
        
        xc /= n;
        yc /= n;
    }
    
    NUMERIC_POINTER(labpt)[0] = xc;
    NUMERIC_POINTER(labpt)[1] = yc;
    
    GEOSGeom_destroy_r(GEOShandle, centroid);
    GEOSGeom_destroy_r(GEOShandle, p);
    
    // Get ring direction
    SEXP ringDir;
    PROTECT(ringDir = NEW_INTEGER(1)); pc++;
    INTEGER_POINTER(ringDir)[0] = hole ? -1 : 1;
    
    // Get hole status
    SEXP Hole;
    PROTECT(Hole = NEW_LOGICAL(1)); pc++;
    LOGICAL_POINTER(Hole)[0] = hole;
    
    SEXP ans, cls;
// rchk PROTECT MAKE_CLASS RSB 180602
    PROTECT(cls = MAKE_CLASS("Polygon")); pc++;
    PROTECT(ans = NEW_OBJECT(cls)); pc++;    
    SET_SLOT(ans, install("ringDir"), ringDir);
    SET_SLOT(ans, install("labpt"), labpt);
    SET_SLOT(ans, install("area"), area);
    SET_SLOT(ans, install("hole"), Hole);
    SET_SLOT(ans, install("coords"), crd);
    
    SEXP valid;
    PROTECT(valid = SP_PREFIX(Polygon_validate_c)(ans)); pc++;
    if (!isLogical(valid)) {
        UNPROTECT(pc);
        if (isString(valid)) 
            error(CHAR(STRING_ELT(valid, 0)));
        else 
            error("invalid Polygon object");
    }
    
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_geospoint2SpatialPoints(SEXP env, GEOSGeom geom, SEXP p4s, SEXP id, int n) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
        
    int type = GEOSGeomTypeId_r(GEOShandle, geom);    
    if ( type != GEOS_POINT && type != GEOS_MULTIPOINT && type != GEOS_GEOMETRYCOLLECTION )
        error("rgeos_geospoint2SpatialPoints: invalid geometry type");
    
    int pc=0;
    SEXP bbox, crdmat;
    if (GEOSisEmpty_r(GEOShandle, geom))
        error("rgeos_geospoint2SpatialPoints: empty point found");
    //if (GEOSisEmpty_r(GEOShandle, geom)==0) {
        PROTECT(bbox = rgeos_geom2bbox(env, geom)); pc++;
        PROTECT(crdmat = rgeos_geospoint2crdMat(env, geom, id, n, type)); pc++;
    //} else {
    //    bbox = R_NilValue;
    //    crdmat = R_NilValue;
    //}
    
    SEXP ans, cls;
// rchk PROTECT MAKE_CLASS RSB 180602
    PROTECT(cls = MAKE_CLASS("SpatialPoints")); pc++;
    PROTECT(ans = NEW_OBJECT(cls)); pc++;    
    SET_SLOT(ans, install("coords"), crdmat);
    SET_SLOT(ans, install("bbox"), bbox);
    SET_SLOT(ans, install("proj4string"), p4s);

    UNPROTECT(pc);
    return(ans);
}


SEXP rgeos_geosline2SpatialLines(SEXP env, GEOSGeom geom, SEXP p4s, SEXP idlist, int nlines) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    GEOSGeom curgeom;
    GEOSGeom subgeom;
    GEOSCoordSeq s;
    int type = GEOSGeomTypeId_r(GEOShandle, geom);
    if (type != GEOS_LINESTRING && type != GEOS_MULTILINESTRING && 
        type != GEOS_LINEARRING && type != GEOS_GEOMETRYCOLLECTION ) {

        error("rgeos_geosline2SpatialLines: invalid type");
    }
    if (nlines < 1) error("rgeos_geosline2SpatialLines: invalid number of geometries");
    
    int pc=0;

    if (nlines > length(idlist))
        error("rgeos_geosline2SpatialLines: nlines > length(idlist)");

    SEXP bbox, lines_list, cls_Line, cls_Lines;
    PROTECT(bbox = rgeos_geom2bbox(env, geom)); pc++;
    PROTECT(lines_list = NEW_LIST(nlines)); pc++;
    PROTECT(cls_Line = MAKE_CLASS("Line")); pc++;
    PROTECT(cls_Lines = MAKE_CLASS("Lines")); pc++;

    
    for(int j = 0; j < nlines; j++) {

        int jpc = 0;
        
        curgeom = (type == GEOS_GEOMETRYCOLLECTION) ?
                                (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, j) :
                                geom;
        if (curgeom == NULL) 
            error("rgeos_geosline2SpatialLines: unable to get geometry collection geometry");
        int curtype = GEOSGeomTypeId_r(GEOShandle, curgeom);
        
        int n = GEOSGetNumGeometries_r(GEOShandle, curgeom);
        if (n == -1) error("rgeos_geosline2SpatialLines: invalid number of geometries in current geometry");
        n = n ? n : 1;
        
        SEXP line_list;
        PROTECT(line_list = NEW_LIST(n)); jpc++;
        
        for(int i = 0; i < n; i++) {

            int ipc = 0;

            subgeom = (curtype == GEOS_MULTILINESTRING && !GEOSisEmpty_r(GEOShandle, curgeom)) ?
                                    (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, curgeom, i) :
                                    curgeom;
            if(subgeom == NULL) error("rgeos_geosline2SpatialLines: unable to get subgeometry");
            
            SEXP crdmat;
            if (GEOSisEmpty_r(GEOShandle, subgeom) == 0) {
                s = (GEOSCoordSeq) GEOSGeom_getCoordSeq_r(GEOShandle, subgeom);
                if (s == NULL) 
                    error("rgeos_geosline2SpatialLines: unable to generate coordinate sequence");

                PROTECT( crdmat = rgeos_CoordSeq2crdMat(env, s, FALSE, FALSE)); ipc++;
            } else {
                error("rgeos_geosline2SpatialLines: empty line found");
//                PROTECT( crdmat = R_NilValue);
            }

            SEXP line;
// rchk PROTECT MAKE_CLASS RSB 180602
            PROTECT(line = NEW_OBJECT(cls_Line)); ipc++;
            SET_SLOT(line, install("coords"), crdmat);
            SET_VECTOR_ELT(line_list, i, line );
        
            UNPROTECT(ipc);
        }

        SEXP lines;
// rchk PROTECT MAKE_CLASS RSB 180602
        PROTECT( lines = NEW_OBJECT(cls_Lines) ); jpc++;
        SET_SLOT(lines, install("Lines"), line_list);
        
        char idbuf[BUFSIZ];
        strcpy(idbuf, CHAR( STRING_ELT(idlist, j) ));
        
        SEXP id;
        PROTECT( id = NEW_CHARACTER(1) ); jpc++;
        SET_STRING_ELT(id, 0, COPY_TO_USER_STRING(idbuf));
        SET_SLOT(lines, install("ID"), id);

        SET_VECTOR_ELT( lines_list, j, lines );
        
        UNPROTECT(jpc);
    }
    
    SEXP ans;    
    PROTECT(ans = NEW_OBJECT(MAKE_CLASS("SpatialLines"))); pc++;    
    SET_SLOT(ans, install("lines"), lines_list);
    SET_SLOT(ans, install("bbox"), bbox);
    SET_SLOT(ans, install("proj4string"), p4s);


    UNPROTECT(pc);
    return(ans);
}


SEXP rgeos_geosring2SpatialRings(SEXP env, GEOSGeom geom, SEXP p4s, SEXP idlist, int nrings) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    int type = GEOSGeomTypeId_r(GEOShandle, geom);
    if (type != GEOS_LINEARRING && type != GEOS_GEOMETRYCOLLECTION )
        error("rgeos_geosring2SpatialRings: invalid type");
    
    if (nrings < 1) error("rgeos_geosring2SpatialRings: invalid number of geometries");
    
    int pc=0;
    SEXP bbox, rings_list, cls_Ring;
    PROTECT(bbox = rgeos_geom2bbox(env, geom)); pc++;
    PROTECT(rings_list = NEW_LIST(nrings)); pc++;
    PROTECT(cls_Ring = MAKE_CLASS("Ring")); pc++;

    for(int j = 0; j < nrings; j++) {

        int lpc = 0;

        GEOSGeom curgeom = (type == GEOS_GEOMETRYCOLLECTION) ?
                                (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, j) :
                                geom;
        if (curgeom == NULL) {
            UNPROTECT(pc);
            error("rgeos_geosring2SpatialRings: unable to get geometry collection geometry");
        }
        
        SEXP crdmat;
        if (GEOSisEmpty_r(GEOShandle, curgeom) == 0) {
            GEOSCoordSeq s = (GEOSCoordSeq) GEOSGeom_getCoordSeq_r(GEOShandle, curgeom);
            if (s == NULL) {
                UNPROTECT(pc);
                error("rgeos_geosring2SpatialRings: unable to generate coordinate sequence");
            }
            PROTECT(crdmat = rgeos_crdMatFixDir(PROTECT(rgeos_CoordSeq2crdMat(env, s, FALSE, FALSE)), FALSE)); lpc += 2;
        } else {
            PROTECT( crdmat = R_NilValue); lpc++;
        }
        
        SEXP ring;
        PROTECT(ring = NEW_OBJECT(cls_Ring)); lpc++;   
        SET_SLOT(ring, install("coords"), crdmat);
        
        SEXP id;
        PROTECT( id = NEW_CHARACTER(1) ); lpc++;
        char idbuf[BUFSIZ];
        strcpy(idbuf, CHAR( STRING_ELT(idlist, j) ));
        SET_STRING_ELT(id, 0, COPY_TO_USER_STRING(idbuf));
        
        SET_SLOT(ring, install("ID"), id);

        SET_VECTOR_ELT(rings_list, j, ring );
        
        
        UNPROTECT(lpc);
    }
    
    SEXP ans, cls;
    PROTECT(cls = MAKE_CLASS("SpatialRings")); pc++;
    PROTECT(ans = NEW_OBJECT(cls)); pc++;    
    SET_SLOT(ans, install("rings"), rings_list);
    SET_SLOT(ans, install("bbox"), bbox);
    SET_SLOT(ans, install("proj4string"), p4s);

    UNPROTECT(pc);
    return(ans);
}

