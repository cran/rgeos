calc.labpt.strings = function(pol, label, gridpoints = 50, cex = 1, ...) {
  ## A few functions we’ll make use of later
  
  # Create a rectangular polygon with a given size, centred at a given position
  makerect = function(x, y, wd, ht, rectID)
    Polygons(list(Polygon(cbind(c(x-wd/2,x+wd/2,x+wd/2,x-wd/2,x-wd/2), 
                                c(y-ht/2,y-ht/2,y+ht/2,y+ht/2,y-ht/2)))), rectID)

  # Calculate the distance from the given coordinates (2-column matrix or vector)
  # to the polygon boundary. Returns 0 for any coordinates not fully inside the polygon.
  getDists = function(co) {

    # Convert coordinates to matrix (in case they were a vector)
    co = matrix(co, ncol = 2)
    
    # Create a candidate label rectangle for each coordinate
    rects = SpatialPolygons(sapply(seq_len(nrow(co)),
                          function(i) makerect(co[i,1], co[i,2], wd, ht, i)),
                          proj4string = CRS(proj4string(pol)))
    
    # Check each rectangle to see if it’s inside the polygon
    inside = apply(gContains(pol, rects, byid = TRUE), 1, any)

    # Calculate and return the distance to the polygon boundary
    # for each rectangle (0 for any rectangle (partially) outside the polygon)
    di = numeric(nrow(co))
    if(any(inside))
      di[inside] = gDistance(pol.l, rects[inside], byid = TRUE)
    di
  }
  
  # Fetch the label size
  wd = strwidth(label, cex = cex, ...)
  ht = strheight(label, cex = cex, ...)
  
  # Convert the polygon to lines, so that we can easily measure
  # the distance from points to the polygon boundary
  pol.l = as(pol, "SpatialLines")
  
  # Sample a regular grid of points inside the polygon
  co = coordinates(spsample(pol, n = gridpoints, type = "regular"))

  # Calculate the distances to the polygon boundary
  di = getDists(co)
  
  # Stop if no rectangles were inside the polygon
  if(all(di == 0))
    stop("Could not fit label inside polygon (with the current number of gridpoints)")
  
  # Use numerical optimisation to find better coordinates,
  # with the points furthest from the boundary as inital values
  coinit = co[which.max(di),]
  obscale = sqrt(sum(apply(bbox(pol),1,diff)^2)) # diameter of bounding box
  optres = optim(coinit, getDists, method = "Nelder-Mead", 
               control = list(trace = FALSE, fnscale = -obscale, reltol = .01))
  optval = optres$par # ‘optimal’ label position
  
  # Only return the calculated position of the numerical optimisation succeeded
  if(optres$convergence != 0) {
    warning("Numerical optimisation did not converge. Returning result of grid search.")
    optval = coinit
  }
  # Return the ‘optimal’ label position
  optval
}

polygonsLabel = function(pol, labels, ...) {
    ret = matrix(0, length(labels), 2)
    for (i in seq(along=labels))
        ret[i,] = calc.labpt.strings(pol[i,], labels[i], ...)
    ret
}
