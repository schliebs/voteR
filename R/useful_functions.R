#' Function %nin%
#' @description The opposite of %in%
#' @return Logical Vector
#' @examples
#' c(2,4,8) %nin% c(1,4,7)
#' @section Warning:
#' Do not operate heavy machinery within 8 hours of using this function.
`%nin%` <- function(x,y) ! x %in% y

