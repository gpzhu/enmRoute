#' Ranking polygon patch by carrying capacity, result of which used to determine parameter r in enmRoute
#' @import terra sf units smoothr exactextractr
#' @export
#' @param pred1 Ecological Niche Model Suitability Suitability Prediction, ranging 0-1000
#' @param canD candidate polygons of patches for optimization
#' @param p  small size of pieces (km2) removing before optimization
#'
rankCI<-function(pred1, canD, p){

  ### remove small pieces ###
  area_thresh <- units::set_units(p, km^2)
  canD<-st_as_sf(canD)
  shape <- drop_crumbs(canD, threshold = area_thresh)

  ### calculate  CI ####
  sm <-st_as_sf(shape)

  bb <- exact_extract(pred1, sm,  c('sum','count'))

  bb<-as.matrix(bb)
  colnames(bb)<-c("Capacity Index","Patch Size")
  sm$Capacity<-bb[,1]
  sm$Patch_size<-bb[,2]

  ### rank and return ###
  sm$Rank<-rank(sm$Capacity)

  return(sm)
}
