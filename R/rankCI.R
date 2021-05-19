#' Ranking polygon patch, result of which used to determine parameter r in enmRoute
#' @export
#' @import raster sf units rgeos GISTools rgdal geosphere smoothr exactextractr osrm maptiles mapsf
#' @param pred ENM binary prediction
#' @param p Optimization survey route by removing small size of pieces (km2)
#'
rankCI<-function(pred, p){

  ### transform into polygon ###
  shape1 <- rasterToPolygons(pred,fun=function(x){x == 1},dissolve=TRUE)
  shape2 <- disaggregate(shape1)
  ### remove small pieces ###
  area_thresh <- units::set_units(p, km^2)
  shape3 <- drop_crumbs(shape2, threshold = area_thresh)

  ### calculate  CI ####
  sm <-st_as_sf(shape3)

  bb <- exact_extract(r, sm,  c('sum','count'))
  bb<-as.matrix(bb)
  colnames(bb)<-c("Capacity Index","Patch Size")
  sm$Capacity<-bb[,1]
  sm$Patch_size<-bb[,2]

  ### rank and return ###
  sm$Rank<-rank(sm$Capacity)
  return(sm)
}
