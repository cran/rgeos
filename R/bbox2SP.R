bbox2SP <- function(n, s, w ,e ,bbox=NA, proj4string=CRS("+init=epsg:4326")){
  if (all(!is.na(bbox), is.matrix(bbox), dim(bbox)==c(2,2))) {
      n<-bbox[2,2]
      s<-bbox[2,1]
      w<-bbox[1,1]
      e<-bbox[1,2]
    }
  box1 <- matrix(c(w, s, w, n, e, n, e, s, w, s), ncol=2, byrow=TRUE)
  box1 <- SpatialPolygons(list(Polygons(list(Polygon(box1)),ID=1)),
      proj4string=proj4string)
  box1
}
