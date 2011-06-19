#ifndef RGEOS_H
#define RGEOS_H

#include <R.h>
#include <Rdefines.h>

#include <geos_c.h>

/* use same define in package's local_stubs.c file */
#define SP_XPORT(x) RGEOS_ ## x
#include "sp.h"

#define R_OFFSET 1

// Utility functions
SEXP rgeos_GEOSversion(void);
SEXP rgeos_Init(void);
SEXP rgeos_finish(SEXP env);
static void rgeos_finish_handle(SEXP ptr);
GEOSContextHandle_t getContextHandle(SEXP env);


double getScale(SEXP env);
double makePrecise(double val, double scale);
double sym_round(double val);
double java_math_round(double val);
double rint_vc(double val);
// Based on geos rounding methods, use just one global 
// round function, so it can be easily switched globally 
double rgeos_round(double val);
// inline removed, see Writing R extensions, section 6.14
// http://cran.r-project.org/doc/manuals/R-exts.html#Inlining-C-functions

SEXP rgeos_double_translate(SEXP env, SEXP obj, SEXP id);

SEXP rgeos_PolyCreateComment(SEXP env, SEXP pls);

// Bounding Box functions - rgeos_bbox.c
SEXP rgeos_geom2bbox(SEXP env, GEOSGeom geom);


// Coordinate sequence and matrix functions - rgeos_coord.c

void     rgeos_Pt2xy(SEXP env, GEOSGeom point, double *x, double *y);
GEOSGeom rgeos_xy2Pt(SEXP env, double x, double y);

GEOSCoordSeq rgeos_crdMat2CoordSeq(SEXP env, SEXP mat, SEXP dim);
GEOSGeom rgeos_crdMat2LineString(SEXP env, SEXP mat, SEXP dim);
GEOSGeom rgeos_crdMat2LinearRing(SEXP env, SEXP mat, SEXP dim);
GEOSGeom rgeos_crdMat2Polygon(SEXP env, SEXP mat, SEXP dim);

SEXP rgeos_CoordSeq2crdMat(SEXP env, GEOSCoordSeq s, int HasZ, int rev);
SEXP rgeos_geospoint2crdMat(SEXP env, GEOSGeom geom, SEXP idlist, int ntotal, int type);

SEXP rgeos_formatcrdMat(SEXP crdMat, int n );
SEXP rgeos_crdMatFixDir(SEXP crd, int hole);



// Translate functions GEOS to R - rgeos_geos2R.c
SEXP rgeos_convert_geos2R(SEXP env, GEOSGeom geom, SEXP p4s, SEXP id);
SEXP rgeos_geospoint2SpatialPoints(SEXP env, GEOSGeom mpt, SEXP p4s, SEXP id, int n);
SEXP rgeos_geosline2SpatialLines(SEXP env, GEOSGeom geom, SEXP p4s, SEXP id, int ng);
SEXP rgeos_geosring2SpatialRings(SEXP env, GEOSGeom geom, SEXP p4s, SEXP idlist, int nrings);

SEXP rgeos_geospolygon2SpatialPolygons(SEXP env, GEOSGeom geom, SEXP p4s, SEXP IDs, int ng);
SEXP rgeos_geospolygon2Polygons(SEXP env, GEOSGeom geom, SEXP id);
SEXP rgeos_geosring2Polygon(SEXP env, GEOSGeom lr, int hole);


//Translate functions R to GEOS - rgeos_R2geos.c
GEOSGeom rgeos_convert_R2geos(SEXP env, SEXP obj);

GEOSGeom rgeos_SpatialPoints2geospoint(SEXP env, SEXP obj);
GEOSGeom rgeos_SpatialLines2geosline(SEXP env, SEXP obj);
GEOSGeom rgeos_Lines2geosline(SEXP env, SEXP obj);
GEOSGeom rgeos_SpatialRings2geosring(SEXP env, SEXP obj);

GEOSGeom rgeos_SpatialPolygons2geospolygon(SEXP env, SEXP obj);
GEOSGeom rgeos_Polygons2geospolygon(SEXP env, SEXP obj);
GEOSGeom rgeos_Polygons2MP(SEXP env, SEXP obj);
GEOSGeom rgeos_Lines2MP(SEXP env, SEXP obj);
GEOSGeom rgeos_Polygon2MP(SEXP env, SEXP obj);
GEOSGeom rgeos_Polygons_i_2Polygon(SEXP env, SEXP pls, SEXP vec);


// WKT Functions - rgeos_wkt.c
SEXP rgeos_readWKT(SEXP env, SEXP obj, SEXP p4s, SEXP id);
SEXP rgeos_writeWKT(SEXP env, SEXP obj, SEXP byid);


// Topology Functions - rgeos_topology.c

SEXP rgeos_envelope(SEXP env, SEXP obj, SEXP id, SEXP byid);
SEXP rgeos_convexhull(SEXP env, SEXP obj, SEXP id, SEXP byid);
SEXP rgeos_boundary(SEXP env, SEXP obj, SEXP id, SEXP byid); 
SEXP rgeos_getcentroid(SEXP env, SEXP obj, SEXP id, SEXP byid);
SEXP rgeos_pointonsurface(SEXP env, SEXP obj, SEXP id, SEXP byid);
SEXP rgeos_linemerge(SEXP env, SEXP obj, SEXP id, SEXP byid);
SEXP rgeos_unioncascaded(SEXP env, SEXP obj, SEXP id, SEXP byid );
#ifdef HAVEUNARYUNION
SEXP rgeos_unaryunion(SEXP env, SEXP obj, SEXP id, SEXP byid );
#endif
SEXP rgeos_topologyfunc(SEXP env, SEXP obj, SEXP id, SEXP byid, 
                        GEOSGeom (*topofunc)(GEOSContextHandle_t, const GEOSGeom) );

SEXP rgeos_simplify(SEXP env, SEXP obj, SEXP tol, SEXP id, SEXP byid, SEXP topPres);
SEXP rgeos_polygonize(SEXP env, SEXP obj, SEXP id, SEXP p4s, SEXP cutEdges);

// Binary Topology Functions - rgeos_topology_binary.c

SEXP rgeos_difference(SEXP env, SEXP geom1, SEXP geom2, SEXP byid, SEXP ids);
SEXP rgeos_symdifference(SEXP env, SEXP geom1, SEXP geom2, SEXP byid, SEXP ids);
SEXP rgeos_intersection(SEXP env, SEXP geom1, SEXP geom2, SEXP byid, SEXP ids);
SEXP rgeos_union(SEXP env, SEXP geom1, SEXP geom2, SEXP byid, SEXP ids);
SEXP rgeos_binarytopologyfunc(SEXP env, SEXP geom1, SEXP geom2, SEXP byid, SEXP ids,
                              GEOSGeom (*bintopofunc)(GEOSContextHandle_t, const GEOSGeom, const GEOSGeom));


// Binary Predicate Functions - rgeos_predicate_binary.c

SEXP rgeos_intersects_prepared(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid);
SEXP rgeos_contains_prepared(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid);
SEXP rgeos_containsproperly_prepared(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid);
SEXP rgeos_covers_prepared(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid);

SEXP rgeos_binpredfunc_prepared(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, 
		char (*binpredfunc_prepared)(GEOSContextHandle_t, const GEOSPreparedGeometry* pg1, const GEOSGeometry* g2));

SEXP rgeos_disjoint(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid);
SEXP rgeos_touches(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid);
SEXP rgeos_intersects(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid);
SEXP rgeos_crosses(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid);
SEXP rgeos_within(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid);
SEXP rgeos_contains(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid);
SEXP rgeos_overlaps(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid);
SEXP rgeos_equals(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid);
SEXP rgeos_relate(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid);
SEXP rgeos_binpredfunc( SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, 
                        char (*binpredfunc)(GEOSContextHandle_t, const GEOSGeom, const GEOSGeom));

SEXP rgeos_equalsexact(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP tol, SEXP byid);
SEXP rgeos_relatepattern(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP pattern, SEXP byid);
SEXP rgeos_binpredfunc_opt(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP opt, SEXP byid, int relpat);


// Unary Predicate Functions - rgeos_predicate_unary.c

SEXP rgeos_isvalid(SEXP env, SEXP spgeom, SEXP byid);
SEXP rgeos_isvalidreason(SEXP env, SEXP spgeom, SEXP byid);
SEXP rgeos_issimple(SEXP env, SEXP spgeom, SEXP byid);
SEXP rgeos_isring(SEXP env, SEXP spgeom, SEXP byid);
SEXP rgeos_hasz(SEXP env, SEXP spgeom, SEXP byid);
SEXP rgeos_isempty(SEXP env, SEXP spgeom, SEXP byid);
SEXP rgeos_unarypredfunc(SEXP env, SEXP spgeom, SEXP byid,
                         char (*unarypredfunc)(GEOSContextHandle_t, const GEOSGeom));

// Buffer Functions - rgeos_buffer.c
SEXP rgeos_buffer(SEXP env, SEXP obj, SEXP byid, SEXP id, SEXP width, SEXP quadsegs, 
                  SEXP capStyle, SEXP joinStyle, SEXP mitreLimit);


// Miscelaneous functions - rgeos_misc.c

SEXP rgeos_area(SEXP env, SEXP obj, SEXP byid);
SEXP rgeos_length(SEXP env, SEXP obj, SEXP byid);
SEXP rgeos_miscfunc(SEXP env, SEXP obj, SEXP byid, int (*miscfunc)(GEOSContextHandle_t, const GEOSGeom, double *) );

SEXP rgeos_distance(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid);
SEXP rgeos_hausdorffdistance(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid);
SEXP rgeos_distancefunc(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, 
                        int (*distfunc)(GEOSContextHandle_t,const GEOSGeom,const GEOSGeom, double *));

SEXP rgeos_hausdorffdistancedensify(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP densifyFrac, SEXP byid);
SEXP rgeos_distancedensifyfunc(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP densifyFrac, SEXP byid, 
                                int (*distfunc)(GEOSContextHandle_t,const GEOSGeom,const GEOSGeom, double, double *));

// GPC functions
GEOSGeom GPCptPolygon(SEXP env, SEXP obj);
GEOSGeom GPCpt2LinearRing(SEXP env, SEXP obj);
GEOSCoordSeq GPCpt2CoordSeq(SEXP env, SEXP obj);
GEOSGeom GCPPtsGC(SEXP env, SEXP pls);
GEOSGeom GPCpt_i_Polygon(SEXP env, SEXP pls, SEXP vec);


SEXP checkHolesGPC(SEXP env, SEXP A);

/* SEXP GCpolysGPCpts(SEXP env, GEOSGeom GC); */
SEXP GCGCPPts(SEXP env, GEOSGeom Geom);
SEXP rgeos_LinearRingGCPPts(SEXP env, GEOSGeom lr, int hole);


// Needs to be classified


SEXP rgeos_PolygonsContain(SEXP env, SEXP obj);
SEXP GC_Contains(SEXP env, GEOSGeom GC);
SEXP rgeos_poly_findInBox(SEXP env, SEXP pls, SEXP as_points);

// STRtree functions

SEXP rgeos_binary_STRtree_query(SEXP env, SEXP obj1, SEXP obj2);
SEXP rgeos_unary_STRtree_query(SEXP env, SEXP obj);

#endif

