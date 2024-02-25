#' Generate candidate patches/polygons from binary niche model predictions and/or buffered introduced distributional records
#' @export
#' @import sf units terra
#' @param pred2 ENM binary prediction
#' @param obs occurrence records of longitude and latitude, input as geographic coordinate system (decimal degrees), WGS 1984
#' @param b distance for buffering, unit is meter
canD<-function(pred2, obs, b){
              if (missing(obs) & missing(b)) {
                 polyA <- as.polygons(patches(pred2 > 1))
                 return(polyA)
              } else {
      ### transform into polygon ###
      polyA <- as.polygons(patches(pred2 > 1))
      ### preparing observation buffering then binding with patches ####
      obs<-vect(obs, geom=c("longitude", "latitude"), crs="+proj=longlat +datum=WGS84")
      ## buffering
      polyB<-buffer(obs, width=b)

      ##### Get the overlap ##########
      cc<-union(polyA, polyB)

      cc<-aggregate(cc, "patches")

      cc$Capacity<-"1"

      cc<-st_as_sf(cc)

      return(cc)

  }
}
