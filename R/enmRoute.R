#' Survey_route_optimization
#' @import terra sf units smoothr exactextractr osrm
#' @export
#' @importFrom terra na.omit
#' @param pred1 Ecological Niche Model Suitability Prediction
#' @param pred2 Ecological Niche Model Suitability Binary Prediction
#' @param p Optimization by removing small size of pieces
#' @param r Optimization by remove low ranked patches
#' @param obs occurrence records of longitude and latitude, input as geographic coordinate system (decimal degrees), WGS 1984
#' @param b distance for buffering, unit is kilometer
enmRoute<-function(pred1, pred2, p, r, obs, b){
  if (missing(obs) & missing(b)) {
    ### transform into polygon ###
    polyA <- as.polygons(patches(pred2 > 1))

    ### remove small pieces ###
    area_thresh <- units::set_units(p, km^2)
    shape <- drop_crumbs(polyA, threshold = area_thresh)

    ### calculate and ranking CI ####
    sm <-st_as_sf(shape)
    bb <- exact_extract(pred1, sm, 'sum')
    bb<-as.matrix(bb)
    colnames(bb)<-"Capacity Index"
    sm$Capacity<-bb
    sm$Rank<-rank(sm$Capacity)

    ### remove CI low-ranked patches ###
    sub<-subset(sm,Rank > r)

    ### set the trip ###
    ct<-st_centroid(sub)
    ctxy<-as.data.frame(st_coordinates(ct))
    trips <- osrmTrip(loc = ctxy, returnclass = "sf")
    mytrip <- trips[[1]]$trip

    ####return
    return(mytrip)
    } else {
#######################################################################
##### assemble patches from buffered occ and model prediction #########
    polyA <- as.polygons(patches(pred2 > 1))

    ### preparing observation buffering then binding with habitat suitability patches ####
    obs<-vect(obs, geom=c("longitude", "latitude"), crs="+proj=longlat +datum=WGS84")
    ## buffering
    polyB<-buffer(obs, width=b)

    ##### Get the overlap ##########
    cc<-union(polyA, polyB)
    cc<-aggregate(cc, "patches")

    cc$Capacity<-"1"
    cc<-st_as_sf(cc)

############# then remove small pieces after assembling ############
    area_thresh <- units::set_units(p, km^2)
    sm <- drop_crumbs(cc, threshold = area_thresh)

    ############### calculate and ranking CI #####
    bb <- exact_extract(pred1, sm, 'sum')
    bb<-as.matrix(bb)
    colnames(bb)<-"Capacity Index"
    sm$Capacity<-bb
    sm$Rank<-rank(sm$Capacity)

    ###### remove CI low-ranked patches ##########
    pm<-subset(sm,Rank > r)

    ######### set the trip #######################
    pm <-st_as_sf(pm)
    ct<-st_centroid(pm)
    ctxy<-as.data.frame(st_coordinates(ct))
    trips <- osrmTrip(loc = ctxy, returnclass = "sf")
    
    #### get the trip
    mytrip <- trips[[1]]$trip

    ### return
    return(mytrip)
  }
}

