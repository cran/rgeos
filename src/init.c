#include <R.h>
#include <Rinternals.h>
#include "rgeos.h"

#include <R_ext/Rdynload.h>

// TODO - do any of these functions need to be called from R?

static const R_CMethodDef CEntries[] = {
    {NULL, NULL, 0} 
};


static R_CallMethodDef CallEntries[] = {
    
    //Utility Functions
    {"rgeos_Init", (DL_FUNC) &rgeos_Init, 0},
    {"rgeos_finish", (DL_FUNC) &rgeos_finish, 1},
    {"rgeos_GEOSversion", (DL_FUNC) &rgeos_GEOSversion, 1},
    {"rgeos_double_translate", (DL_FUNC) &rgeos_double_translate, 4},
    {"rgeos_PolyCreateComment", (DL_FUNC) &rgeos_PolyCreateComment, 2},
    {"rgeos_sp_linkingTo_version", (DL_FUNC) &rgeos_sp_linkingTo_version, 0},

    
    //WKT Functions
    {"rgeos_readWKT", (DL_FUNC) &rgeos_readWKT,4}, 
    {"rgeos_writeWKT", (DL_FUNC) &rgeos_writeWKT, 3}, 
    
    //Topology Functions
    {"rgeos_envelope", (DL_FUNC) &rgeos_envelope, 4},
    {"rgeos_convexhull", (DL_FUNC) &rgeos_convexhull, 4},
    {"rgeos_boundary", (DL_FUNC) &rgeos_boundary, 4},
    {"rgeos_getcentroid", (DL_FUNC) &rgeos_getcentroid, 4},
    {"rgeos_pointonsurface", (DL_FUNC) &rgeos_pointonsurface, 4},
    {"rgeos_linemerge", (DL_FUNC) &rgeos_linemerge, 4},
    {"rgeos_unioncascaded", (DL_FUNC) &rgeos_unioncascaded, 4},
#ifdef HAVE_UNARYUNION
    {"rgeos_unaryunion", (DL_FUNC) &rgeos_unaryunion, 4},
#endif
#ifdef HAVE_DELAUNAY
    {"rgeos_delaunaytriangulation", (DL_FUNC) &rgeos_delaunaytriangulation, 4},
#endif
#ifdef HAVE_MAKEVALID
    {"rgeos_makevalid", (DL_FUNC) &rgeos_makevalid, 4},
#endif
#ifdef HAVE_MAKEVALIDPARAMS
    {"rgeos_makevalidparams", (DL_FUNC) &rgeos_makevalidparams, 4},
#endif
#ifdef HAVE_COVERAGEUNION
    {"rgeos_coverageunion", (DL_FUNC) &rgeos_coverageunion, 4},
#endif
#ifdef HAVE_MINIMUMROTATEDRECTANGLE
    {"rgeos_minimumrotatedrectangle", (DL_FUNC) &rgeos_minimumrotatedrectangle, 4},
#endif
#ifdef HAVE_MAXIMUMINSSCRIBEDCIRCLE
    {"rgeos_maximuminscribedcircle", (DL_FUNC) &rgeos_maximuminscribedcircle, 5},
#endif

    {"rgeos_simplify", (DL_FUNC) &rgeos_simplify, 6},
    {"rgeos_polygonize", (DL_FUNC) &rgeos_polygonize, 5},
#ifdef HAVE_NODE
    {"rgeos_node", (DL_FUNC) &rgeos_node, 2},
#endif

    //Binary Topology Functions
    {"rgeos_difference", (DL_FUNC) &rgeos_difference, 5},
    {"rgeos_symdifference", (DL_FUNC) &rgeos_symdifference, 5},
    {"rgeos_intersection", (DL_FUNC) &rgeos_intersection, 5},
    {"rgeos_union", (DL_FUNC) &rgeos_union, 5},

    //Binary Predicate Functions
	{"rgeos_intersects_prepared", (DL_FUNC) &rgeos_intersects_prepared, 4},
	{"rgeos_contains_prepared", (DL_FUNC) &rgeos_contains_prepared, 4},
	{"rgeos_containsproperly_prepared", (DL_FUNC) &rgeos_containsproperly_prepared, 4},
	{"rgeos_covers_prepared", (DL_FUNC) &rgeos_covers_prepared, 4},
	
    {"rgeos_disjoint", (DL_FUNC) &rgeos_disjoint, 4},
    {"rgeos_touches", (DL_FUNC) &rgeos_touches, 4},
    {"rgeos_intersects", (DL_FUNC) &rgeos_intersects, 4},
    {"rgeos_crosses", (DL_FUNC) &rgeos_crosses, 4},
    {"rgeos_within", (DL_FUNC) &rgeos_within, 4},
    {"rgeos_contains", (DL_FUNC) &rgeos_contains, 4},
    {"rgeos_overlaps", (DL_FUNC) &rgeos_overlaps, 4},
    {"rgeos_equals", (DL_FUNC) &rgeos_equals, 4},
    {"rgeos_relate", (DL_FUNC) &rgeos_relate, 4},
	{"rgeos_relatepattern", (DL_FUNC) &rgeos_relatepattern, 5},
    {"rgeos_equalsexact", (DL_FUNC) &rgeos_equalsexact, 5},
    
    //Unary Predicate Functions
    {"rgeos_isvalid", (DL_FUNC) &rgeos_isvalid, 3},
    {"rgeos_issimple", (DL_FUNC) &rgeos_issimple, 3},
    {"rgeos_isring", (DL_FUNC) &rgeos_isring, 3},
    {"rgeos_hasz", (DL_FUNC) &rgeos_hasz, 3},
    {"rgeos_isempty", (DL_FUNC) &rgeos_isempty, 3},
    {"rgeos_isvalidreason", (DL_FUNC) &rgeos_isvalidreason, 3},
    
    //Buffer Functions
    {"rgeos_buffer", (DL_FUNC) &rgeos_buffer, 9},

    //Linear referencing functions
    {"rgeos_interpolate", (DL_FUNC) &rgeos_interpolate, 4},
    {"rgeos_project", (DL_FUNC) &rgeos_project, 4},

    //Misc functions
    {"rgeos_area", (DL_FUNC) &rgeos_area, 3},
    {"rgeos_length", (DL_FUNC) &rgeos_length, 3},
    {"rgeos_distance", (DL_FUNC) &rgeos_distance, 4},
    {"rgeos_hausdorffdistance", (DL_FUNC) &rgeos_hausdorffdistance, 4},
    {"rgeos_hausdorffdistancedensify", (DL_FUNC) &rgeos_hausdorffdistancedensify, 5},
#ifdef HAVE_NEARESTPOINTS
    {"rgeos_nearestpoints", (DL_FUNC) &rgeos_nearestpoints, 3},
#endif

    {"rgeos_PolygonsContain", (DL_FUNC) &rgeos_PolygonsContain, 2},
    {"rgeos_poly_findInBox", (DL_FUNC) &rgeos_poly_findInBox, 3}, 
    {"rgeos_binary_STRtree_query", (DL_FUNC) &rgeos_binary_STRtree_query, 3}, 
    {"rgeos_unary_STRtree_query", (DL_FUNC) &rgeos_unary_STRtree_query, 2}, 
    {NULL, NULL, 0}
};

void 
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_rgeos(DllInfo *dll) {

//    SEXP INIT;

    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL); // RSB FIXME
    R_useDynamicSymbols(dll, FALSE);

//    INIT = rgeos_Init();

}
