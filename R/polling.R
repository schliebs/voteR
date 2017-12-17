# Calculate Dirichlet-Polling Error intervals: Empirical Quantile Confidence

c (cdu = 0.5,
   spd = 0.4,
   fdp = 0.1)

sample_n <- 1000

sample_dirichlet <- function(vote = c (cdu = 0.5,
                                       spd = 0.4,
                                       fdp = 0.1),
                             sample_n = 1000,
                             n_draw = 10000){

  draw <- gtools::rdirichlet(n_draw,vote*sample_n)
  draw %>%
    reshape2::melt() %>%
    as.data.frame() %>%
    dplyr::group_by(Var2) %>%
    dplyr::summarise(mean = mean(value,na.rm = TRUE),
                     q05 = quantile(value,0.05,na.rm = TRUE),
                     q95 = quantile(value,0.95,na.rm = TRUE))
}

n_draw <- 1000000
sample_n <- 1000
vote <- c(0.5,0.4,0.1)

draw <- gtools::rdirichlet(n_draw,vote*sample_n)
draw %>%
  reshape2::melt() %>%
  as.data.frame() %>%
  dplyr::group_by(Var2) %>%
  dplyr::summarise(mean = mean(value,na.rm = TRUE),
                   q05 = quantile(value,0.05,na.rm = TRUE),
                   q95 = quantile(value,0.95,na.rm = TRUE))

