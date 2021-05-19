#' Thresholding model predictions
#' @export
#' @import raster
#' @param sdm ENM suitability prediction
#' @param points Occ of long and lat
#' @param type mtp, p10
#' @param threshold a specific threshold value
#' @param binary whether to return the binary prediction
#'
thd <- function(sdm, points = NULL, type = NULL, threshold = NULL, binary = FALSE) {
  if (!is.null(points)) {
    pointVals <- raster::extract(sdm, points)
    if (type == "mtp") {
      threshold <- min(na.omit(pointVals))
    } else if (type == "p10") {
      if (length(pointVals) < 10) {
        p10 <- floor(length(pointVals) * 0.9)
      } else {
        p10 <- ceiling(length(pointVals) * 0.9)
      }
      threshold <- rev(sort(pointVals))[p10]
    }
  }
  raster_thresh <- sdm
  raster_thresh[raster_thresh < threshold] <- NA
  if (binary) {
    raster_thresh[raster_thresh >= threshold] <- 1
  }
  return(raster_thresh)
}


