# Calculate Dirichlet-Polling Error intervals: Empirical Quantile Confidence

#' Dirichlet-sample of a multinomial election poll
#' @description Calculate a dirichlet-sample of a multinomial election poll
#' @param vote A labeled party vote share vector.
#' @param sample_n The number of observations in the poll sample.
#' @param n_draw How many samples to draw from the dirichlet distribution.
#' @return A data frame containing \code{n} rows of samples for each party.
#' @examples
#' sample_dirichlet(vote = c(cdu = 0.33,
#'                           spd = 0.20,
#'                           fdp = 0.11,
#'                           linke = 0.09,
#'                           gruene = 0.09,
#'                           afd = 0.12,
#'                           sonstige = 0.05),
#'                  sample_n = 1000,
#'                  n_draw = 10000)
#' @section Warning:
#' Do not operate heavy machinery within 8 hours of using this function.
#' @export
sample_dirichlet <- function(vote = c(cdu = 0.5,
                                      spd = 0.4,
                                      fdp = 0.1),
                             sample_n = 1000,
                             n_draw = 10000){

  draw <- gtools::rdirichlet(n_draw,vote*sample_n)
  draw %<>%
    as.data.frame()
  colnames(draw) <- names(vote)
  return(draw)
}


#' Empirical Dirichlet Quantiles from Multinomial Election Poll
#' @description  Calculate empirical quantiles from a sample created by \code{\link{sample_dirichlet}} of a multinomial election poll
#' @inheritParams sample_dirichlet
#' @param show_mean Logical T/F: Show sample mean.
#' @param show_quantiles Vector of quantiles/confidence boundaries to calculate.
#' @param round Logical T/F Round Results to k decimal digits.
#' @return A data frame containing \code{n} rows of samples for each party.
#' @examples
#' sample_dirichlet_quantiles(vote = c(cdu = 0.33,
#'                           spd = 0.20,
#'                           fdp = 0.11,
#'                           linke = 0.09,
#'                           gruene = 0.09,
#'                           afd = 0.12,
#'                           sonstige = 0.05),
#'                  sample_n = 1000,
#'                  n_draw = 10000,
#'                  show_mean = TRUE,
#'                  show_quantiles = c(0.05,0.95))
#' @section Warning:
#' Do not operate heavy machinery within 8 hours of using this function.
#' @export
sample_dirichlet_quantiles <- function(vote = c(cdu = 0.5,
                                                spd = 0.4,
                                                fdp = 0.1),
                                       sample_n = 1000,
                                       n_draw = 10000,
                                       show_mean = TRUE,
                                       show_quantiles = c(0.05,0.95),
                                       round = 2){

  samples <- sample_dirichlet(vote = vote,sample_n = sample_n,n_draw = n_draw)
  help1 <- samples %>% tidyr::gather(key = 'party',value = 'result')

  q <- show_quantiles
  quantile_vector <- paste0('q_',as.character(q*100),'= round(quantile(result,c(',q,'),na.rm = TRUE),',round,')') %>%
    paste0(collapse = ",")

  eval(parse(text = paste0('help2 <- help1 %>% dplyr::group_by(party) %>% dplyr::summarise(',
                ifelse(show_mean == TRUE,paste0('mean = round(mean(result,na.rm = TRUE),',round,')',','),''),
                quantile_vector,
                ')'


             )
      )
  )
  return(as.data.frame(help2))
}

# sample_dirichlet_quantiles(vote = c(cdu = 0.5,
#                                     spd = 0.4,
#                                     fdp = 0.1),
#                            sample_n = 1000,
#                            n_draw = 10000,
#                            show_mean = T,
#                            show_quantiles = c(0.10,0.95),
#                            round = 4)
