#' Buffering introduced distributional records
#' @export
#' @import terra
#' @param obs occurrence records of longitude and latitude, input as geographic coordinate system (decimal degrees), WGS 1984
#' @param b distance for buffering, unit is meter

buf<-function(obs,b){
           ## define longitude and latitude
            obs<-vect(obs, geom=c("longitude", "latitude"), crs="+proj=longlat +datum=WGS84")
            ## buffering
            buf<-buffer(obs, width=b)
            ## return buf as vect object
            return(buf)

}
