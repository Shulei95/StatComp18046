# This is an example function named 'SL.ruben.test'
# which get interval estimation in ruben test

#' @title SL.ruben.test
#' @description an example function getting interval estimation in ruben test
#' @param n the number of samples
#' @param r correlation coefficient of sample
#' @param alpha quantile
#' @return a interval estimation
#' @examples
#' SL.ruben.test(6, 0.8)
#' SL.ruben.test(25, 0.7)
#' @export

SL.ruben.test <- function(n, r, alpha=0.05){
  u <- qnorm(1-alpha/2)
  r_star <- r/sqrt(1-r^2)
  a <- 2*n-3-u^2
  b <- r_star*sqrt((2*n-3)*(2*n-5))
  c <- (2*n-5-u^2)*r_star^2-2*u^2
  y1 <- (b-sqrt(b^2-a*c))/a
  y2 <- (b+sqrt(b^2-a*c))/a
  data.frame(n = n, r = r, conf = 1-alpha,
           L = y1/sqrt(1+y1^2), U = y2/sqrt(1+y2^2))
}
