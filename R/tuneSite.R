#' Tune sample site by estimating proportion of patches to be sampled
#' exploring relationship between sampled patches with driving time and accumulated capacity
#' driving time is related to distance between patches, surveying time in a specific patch is scaled with accumulated capacity
#' @export
#' @param shp is the output polygon of rankCI function in this package
#' @param r Optimization by removing low ranked patches, the number of patches to be removed
#' @param u increment of patches to be iteratively removed, values range between 1 and (dim(shp)[1]-5)

tuneSite<-function(shp, r, u){
  ############ ranking and rarefying by patch index ###############
  shape4<-subset(shp,Rank > r)
  shape4$Rank1<-rank(shape4$Capacity)
  mx<-dim(shp)[1]-r-5
  ###### Travel time in minutes and travel distance in kilometers ######
  k<-seq(from = 5, to = mx, by = u)
  kk<-seq(from = mx/100, to = 0.05, by = -u/100)

  t<-matrix(NA, nrow = length(kk), ncol = 5)
  colnames(t)<-c("unit","Driving_time","Proportion_of_patches", "Capacity","Patch_size")
  t[,1]<-k
  t[,3]<-kk
  d<-matrix(NA, nrow = length(kk), ncol = 5)
  colnames(d)<-c("unit","Driving_distance","Proportion_of_patches","Capacity","Patch_size")
  d[,1]<-k
  d[,3]<-kk

  for (i in 1:length(kk)){
    shape5<-subset(shape4,Rank1 > k[i])
    t[i,4]<- sum(shape5$Capacity)
    d[i,4]<-sum(shape5$Capacity)
    t[i,5]<-sum(shape5$Patch_size)
    d[i,5]<-sum(shape5$Patch_size)

    ct<-st_centroid(shape5)
    trips <- osrmTrip(loc = ct, returnclass = "sf")
    mytrip <- trips[[1]]$trip
    t[i,2]<-sum(mytrip$duration)
    d[i,2]<-sum(mytrip$distance)
  }

  tt<-as.data.frame(t)
  dd<-as.data.frame(d)
  cc<-cbind(tt[,3],tt[,2],dd[,2],tt[,4:5])
  colnames(cc)<-c("Proportion_of_patches","Driving_time","Driving_distance","Accumulated_capacity","Patch_size")
  return(cc)
}

