#' Selecting pseudo-absence data for downscale coarse grain climate suitability prediction
#' @export
#' @import raster
#' @param sdm ENM coarse grain climate suitability prediction rescale range from 0-1, whcih is also the extent where regional fine grain model will be calibrated.
#' @param tar fine grain environmental variables for regional habitat suitability modeling.
#' @param n number of pseudo-absence point to be selected.
#'
#########################################################
dsAbs<-function(sdm,tar,n){
  ### resample your coarse grain climate suitability prediction to target fine grain resolution
  pred<-disaggregate(sdm, fact = res(sdm)/res(tar))

  ## calculate the weight
  pred1<-1/(1+(pred/(pred-1))^2)
  ss<-rasterToPoints(pred1)

  ## select pseudo-absence using the weight
  indexes = sample(1:nrow(ss),size=n,prob=ss[,3])
  data = ss[indexes,]
  data<-data[,1:2]
  ## return the data
  return (data)
}
