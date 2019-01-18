# This is an example function named 'SL.area'
# which calculate integrals by Recursive method

#' @title SL.area
#' @description an example function calculating integrals by Recursive method
#' @param f the integrand function
#' @param a the lower limit of integral
#' @param b the upper limit of integral
#' @param eps accurancy
#' @param lim upper bound between partitions
#' @return the result of integrals
#' @examples
#' f <- function(x) 1/x
#' quad<-SL.area(f,1,5); quad
#' @export


SL.area <- function(f, a, b, eps = 1.0e-06, lim = 10) {
  fun1 <- function(f, a, b, fa, fb, a0, eps, lim, fun) {
    d <- (a + b)/2; h <- (b - a)/4;
    fd <- f(d)
    a1 <- h * (fa + fd);
    a2 <- h * (fd + fb)
    if(abs(a0 - a1 - a2) < eps || lim == 0)
      return(a1 + a2)
    else {
      return(fun(f, a, d, fa, fd, a1, eps, lim - 1, fun)
             + fun(f, d, b, fd, fb, a2, eps, lim - 1, fun))
    }
  }
  fa <- f(a); fb <- f(b);
  a0 <- ((fa + fb) * (b - a))/2
  fun1(f, a, b, fa, fb, a0, eps, lim, fun1)
}
