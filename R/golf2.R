library(magrittr)

N <- 19
tries <- c(1443, 694, 455, 353,  272, 256, 240, 217, 200, 237, 202, 192, 174, 
           167, 201, 195, 191, 147, 152)
successes <- c(1346, 577, 337,  208, 149, 136, 111, 69, 67, 75, 52, 46, 54,
               28, 27, 31, 33, 20,  24)
dist <- 2:20 * 12 # converting to inches
golf <- data.frame(tries, successes, dist)

# The golf ball has diameter 2r = 1.68 inches 
r <- 1.68/2
# and the hole has diameter 2R = 4.25 inches
R <- 4.25/2

# estimated parameter from the paper
sigma <- 0.026

theta0 <- function(x) {
  asin((R - r) / x)
}

golf %<>%
  mutate(
    p = successes / tries,
    error_sd = sqrt((p  * (1 - p)) / tries),
    lower = p - 2 * error_sd,
    upper = p + 2 * error_sd,
    fit = 2 * pnorm(theta0(dist) / sigma) - 1
  )

limits <- with(golf, aes(ymax = upper, ymin = lower))
p <- ggplot(golf, aes(x = dist / 12, y = p))
p <- p + geom_pointrange(limits)
p <- p + geom_line(aes(y = fit), colour = "red")
p <- p +  xlab("Distance (feet)") + 
  ylab("Proportion of Success") +
  theme_bw()
p

golf_fit <- stan(file = "golf.stan", iter = 200)
