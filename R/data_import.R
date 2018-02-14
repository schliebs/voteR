# Calculate Dirichlet-Polling Error intervals: Empirical Quantile Confidence

#' Import Run 
#' @description Import Run (Standard: GPX) to Data Frame
#' @param file path and file name.
#' @param layer Which layer you want to select. Use "track_points" for now.
#' @param type Type of log-file: currently, only gpx is supported.
#' @param track_progress T/F if data loading progress tracking is wished for
#' @return A data frame data about XXXXXXX.
#' @examples
#' run <- import_run(file = 'data/2017-11-06.gpx',type = 'gpx')
#' head(run)
#' @section Warning:
#' Do not operate heavy machinery within 8 hours of using this function.
#' @export
import_run <- function(file = 'data/2017-11-06.gpx',
                       layer = 'track_points',
                       type = 'gpx',
                       track_progress = FALSE){
  
  wp <- rgdal::readOGR(file, layer = layer,verbose = track_progress)
  wp$distance <- c(0,sp::spDists(wp, segments=TRUE))
  wp$cumulative.distance <- cumsum(wp$distance)
  wp$time <- lubridate::ymd_hms(wp$time)   
  
  wp %<>%  
    as.data.frame() %>% 
    select(pointID = track_seg_point_id,
           ele,
           time,
           distance,
           cumulative.distance,
           x = coords.x1,
           y = coords.x2)
  
  return(wp)
}
# Layer overview: ogrListLayers(file)



