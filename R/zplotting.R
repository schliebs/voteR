#' @include polling.R


vote = c(cdu = 0.33,
  spd = 0.20,
  fdp = 0.11,
  linke = 0.09,
  gruene = 0.09,
  afd = 0.12,
  sonstige = 0.05)

partycolors_t <- c(spd = "red",
                   cdu = "black",
                   cdu_csu = "black",
                   csu = "darkblue",
                   gruene = "darkgreen",
                   fdp =  "#999900",
                   afd = "blue",
                   piraten = "orange",
                   dielinke = "purple",
                   andere = "black")

partycolors <- c(spd = "red",
                 cdu = "black",
                 cdu_csu = "black",
                 csu = "darkblue",
                 gruene = "green",
                 fdp =  "yellow",
                 afd = "blue",
                 piraten = "orange",
                 dielinke = "purple",
                 andere = "grey")


  ggplot2::ggplot(data = sample_dirichlet_quantiles(vote = vote)) +
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
           width = 0.8)+
  ggplot2::geom_crossbar(aes(x = party,
                    y = mean,
                    color = party,
                    ymin = q_10, ymax = q_95),
                width = 0.8)+
  #### left
    ggplot2::geom_text(aes(x = party ,
                y = q_95 + 0.02,
                col = party,
                label = paste0(round(100*q_10,1)," %")),
            stat = "identity",
            #  hjust = 0.5,
            size = 2,
            alpha = 1.0,
            width = 0.8) +

  ####### right
    ggplot2::geom_text(aes(x = party,
                y = q_95 + 0.08,
                col = party,
                label = paste0(round(100*q_95,1)," %")),
            stat = "identity",
            #  hjust = -0.5,
            size = 2,
            alpha = 1.0,
            width = 0.8) +
  ############# middle
    ggplot2::geom_text(aes(x = party,
                y = q_95 + 0.05,
                col = party,
                label = paste0(round(100*mean,1)," %")),
            stat = "identity",
            size = 4,
            alpha = 1.0,
            width = 0.8) +
    ggplot2::scale_y_continuous(breaks = c(0.1,0.2,0.3,0.4),
                     labels = c("10%","20%","30%","40%")) +
    ggplot2::scale_fill_manual(name = "Partei",
                    values = partycolors)+
    ggplot2::scale_color_manual(name = "Partei",
                     values = partycolors_t)+
    ggplot2::scale_x_discrete(breaks = c("cdu_csu","spd","afd","gruene","dielinke","fdp","andere"),
                   labels = c("Union","SPD","AfD","Gruene","Die Linke","FDP","Andere"))+
  hrbrthemes::theme_ipsum(grid = "Y")+
  ggplot2::theme(legend.position = "none") +
  ggplot2::labs(x = NULL,
                y = NULL,
                title = paste0("Wie wählt BA CME?"),
                subtitle = paste0("Zweitstimmenergebnis mit 90% Konfidenzintervall"),
                caption = paste0("Fallzahl = ",100,"\n90%-Konfidenzintervall berechnet auf Basis von \na posteriori Sampling aus einer Dirichlet-Verteilung\nDurchgeführt von prognosophie.de für Futur 3"))

