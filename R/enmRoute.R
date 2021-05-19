#' Survey_route_optimization
#' @export
#' @export
#' @export
#' @import raster lwgeom sf units rgeos GISTools rgdal geosphere smoothr exactextractr osrm maptiles mapsf
#' @param pred ENM binary prediction
#' @param p Optimization by removing small size of pieces
#' @param r Optimization by remove low ranked patches

enmRoute<-function(pred, p, r){

  ### transform into polygon ###
  shape1 <- rasterToPolygons(pred,fun=function(x){x == 1},dissolve=TRUE)
  shape2 <- disaggregate(shape1)
  ### remove small pieces ###
  area_thresh <- units::set_units(p, km^2)
  shape3 <- drop_crumbs(shape2, threshold = area_thresh)

  ### calculate and ranking CI ####
  sm <-st_as_sf(shape3)
  bb <- exact_extract(pred, sm, 'sum')
  bb<-as.matrix(bb)
  colnames(bb)<-"Capacity Index"
  sm$Capacity<-bb
  sm$Rank<-rank(sm$Capacity)
  ### remove CI low-ranked patches ###
  shape4<-subset(sm,Rank > r)

  ### set the trip ###
  ct<-st_centroid(shape4)

  trips <- osrmTrip(loc = ct, returnclass = "sf")
  mytrip <- trips[[1]]$trip

  ### set the route ###
  #xy<-st_coordinates(ct)
  #pts <- structure(list(x = xy[,1], y = xy[,2]), class = "data.frame", row.names = c(NA, -10L))
  #myroute <- osrmRoute(loc = pts, returnclass = "sf")

  ### return ####
  #return(ct)
  return(mytrip)
  #return(myroute)
}
