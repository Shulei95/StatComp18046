# This is an example function named 'SL.Newtons'
# which solve solutions of nonlinear equations by Newton method

#' @title SL.Newtons
#' @description an example function solving solutions of nonlinear equations by Newton method
#' @param fun functions consisting of equations
#' @param x initial variable
#' @param ep accuracy
#' @param it_max the maximum number of iterations
#' @examples
#' funs<-function(x){
#' f<-c(x[1]^2+x[2]^2-5, (x[1]+1)*x[2]-(3*x[1]+1))
#' J<-matrix(c(2*x[1], 2*x[2], x[2]-3, x[1]+1), nrow=2)
#' list(f=f,J=J)
#' }
#' SL.Newtons(funs, c(0,1))
#' @return solutions of nonlinear equations
#' @export


SL.Newtons<-function (fun, x, ep=1e-5, it_max=100){
  index<-0; k<-1
  while (k<=it_max){
    x1 <- x; obj <- fun(x);
    x <- x - solve(obj$J, obj$f);
    norm <- sqrt((x-x1) %*% (x-x1))
    if (norm<ep){
      index<-1; break
    }
    k<-k+1 }
  obj <- fun(x);
  list(root=x, it=k, index=index, FunVal= obj$f)
}
