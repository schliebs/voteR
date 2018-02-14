#' Summarize_Run
#' @description Get summary statistics for run
#' @param run a run data frame returned by \code{import_run}.
#' @return A ggplot object.
#' @examples
#' run2profile(smooth = 0.5)
#' @section Warning:
#' Do not operate heavy machinery within 8 hours of using this function.
#' @export
summarise_RUN <- function(run = import_run()){
  total_durance <- (run$time[nrow(run)]-run$time[1]) %>% lubridate::as.duration()
  total_durance_seconds <- total_durance %>% as.numeric
  total_durance_hours <- total_durance_seconds/3600
  
  average_speed_kph <- max(run$cumulative.distance)/total_durance_hours
  
  return(average_speed_kph)
}