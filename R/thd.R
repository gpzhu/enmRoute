#' Thresholding ecological niche model suitability predictions
#' @export
#' @import terra sf
#' @param pred1 ENM suitability prediction, ranging 0-1000.
#' @param points obs occurrence records of longitude and latitude, input as geographic coordinate system (decimal degrees), WGS 1984
#' @param type mtp, p10
#' @param threshold a specific threshold value
#' @param binary whether to return the binary prediction
#'
thd <- function(pred1, points = NULL, type = NULL, threshold = NULL, binary = FALSE) {
  if (!is.null(points)) {
    pointVals <- terra::extract(pred1, points)
    pointVals<- pointVals[,2]
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
  rast_thresh <- pred1
  rast_thresh[rast_thresh < threshold] <- NA
  if (binary) {
    rast_thresh[rast_thresh >= threshold] <- 1
  }
  return(rast_thresh)
}

