# Plot a multiparty poll..

#' Plot a multiparty poll
#' @include polling.R
#' @include partycolors.R
#' @include partynames.R
#' @description  ...
#' @param vote A labeled party vote share vector.
#' @param order Method to order parties (Default is "alphabetical"; also takes "descending" and "ascending" as well as manual specification of party vector)
#' @param sample_confidence_bounds Logical T/F: add empirical dirichlet quantiles
#' @inheritParams sample_dirichlet_quantiles
#' @param round Round to k decimals after comma
#' @param xlab x-label string
#' @param ylab y-label string
#' @param title title string
#' @param subtitle subtitle string
#' @param caption caption string
#' @param theme_ipsum Pre-applies nice theme from the hrbrthemes-package.(Attention: possible font-issues when Roboto font is not installed on your computer.)
#' @param grid (Applies only if theme_ipsum == T) Add a grid (options: "none","Y")
#' @return A data frame containing \code{n} rows of samples for each party.
#' @examples
#' sample_dirichlet_quantiles(vote = c(cdu = 0.33,....
#' @section Warning:
#' Do not operate heavy machinery within 8 hours of using this function.
#' @export
plot_poll <- function(vote = c(cdu = 0.33,
                               spd = 0.20,
                               fdp = 0.11,
                               linke = 0.09,
                               gruene = 0.09,
                               afd = 0.12,
                               sonstige = 0.05),
                      order = "alphabetical",
                      sample_confidence_bounds = TRUE,
                      sample_n = 1000,
                      n_draw = 10000,
                      show_quantiles = c(0.05,0.95),
                      round = 1,
                      xlab = "Party",
                      ylab = "Voteshare",
                      title = "Title",
                      subtitle = "Subtitle",
                      caption = "Caption",
                      theme_ipsum = FALSE,
                      grid = "Y"){

  if(sample_confidence_bounds == TRUE){

    gg1 <- ggplot2::ggplot(data = sample_dirichlet_quantiles(vote = vote,
                                                    show_quantiles = show_quantiles,
                                                    round = round + 2)) +
      ggplot2::geom_bar(aes(x = party,
                            y = q_95,
                            fill = party),
                        stat = "identity",
                        alpha = 0.3,
                        width = 0.8)+
      ggplot2::geom_bar(aes(x = party,
                            y = q_10,
                            fill = party),
                        stat = "identity",
                        alpha = 1.0,
                        width = 0.8) +
      ggplot2::geom_crossbar(aes(x = party,
                                 y = mean,
                                 color = party,
                                 ymin = q_10, ymax = q_95),
                             width = 0.8)+
      #### left
      ggplot2::geom_text(aes(x = party ,
                             y = q_95 + 0.02,
                             col = party,
                             label = paste0(round(100*q_10,round),"%")),
                         stat = "identity",
                         #  hjust = 0.5,
                         size = 2,
                         alpha = 1.0) +

      ####### right
      ggplot2::geom_text(aes(x = party,
                             y = q_95 + 0.08,
                             col = party,
                             label = paste0(round(100*q_95,round),"%")),
                         stat = "identity",
                         #  hjust = -0.5,
                         size = 2,
                         alpha = 1.0) +
      ggplot2::geom_text(aes(x = party,
                             y = q_95 + 0.05,
                             col = party,
                             label = paste0(round(100*mean,round),"%")),
                         stat = "identity",
                         size = 4,
                         alpha = 1.0)
    } else
  if(sample_confidence_bounds == FALSE){

      gg1 <- ggplot2::ggplot(data = data.frame(party = names(vote),
                                              mean = vote)) +
        ggplot2::geom_bar(aes(x = party,
                                   y = mean,
                                   color = party),
                          stat = "identity",
                          width = 0.8) +
        ggplot2::geom_text(aes(x = party,
                               y = mean + 0.02,
                               col = party,
                               label = paste0(round(100*mean,round),"%")),
                           stat = "identity",
                           size = 4,
                           alpha = 1.0)
  }



 gg2 <- gg1 +

    ggplot2::scale_y_continuous(breaks = seq(0.05,max(vote,na.rm = TRUE) + 0.15 ,0.05),
                     labels = paste0(100*seq(0.05,max(vote,na.rm = TRUE) + 0.15,0.05),"%")) +
    ggplot2::scale_fill_manual(name = "Partei",
                    values = partycolors)+
    ggplot2::scale_color_manual(name = "Partei",
                     values = partycolors_t)+
    ggplot2::scale_x_discrete(breaks = c("cdu_csu","spd","afd","gruene","dielinke","fdp","andere"),
                    labels = c("Union","SPD","AfD","Gruene","Die Linke","FDP","Andere"))+
  ggplot2::labs(x = xlab,
                y = ylab,
                title = title,
                subtitle = subtitle,
                caption = caption) +
   ggplot2::theme(legend.position = "none")

 if(theme_ipsum == TRUE){
   gg2 <- gg2 +
   hrbrthemes::theme_ipsum(grid = grid)+
   ggplot2::theme(legend.position = "none")}

return(gg2)
}

# still do
# font hrbrthemes
# breaks/partynames (first fix that file)
#order
# add example
# fix documentation issue

#
# plot_poll (vote = c(cdu = 0.33,
#                     spd = 0.20,
#                    fdp = 0.11,
#                    linke = 0.09,
#                    gruene = 0.09,
#                    afd = 0.12,
#                    sonstige = 0.05),
#           sample_confidence_bounds = T,
#           sample_n = 1000,
#           n_draw = 10000,
#           show_quantiles = c(0.10,0.95),
#           round = 1,
#           xlab = "Party",
#           ylab = "Voteshare",
#           title = "Title",
#           subtitle = "Subtitle",
#           caption = "Caption",
#           theme_ipsum = F,
#           grid = "Y")
#

