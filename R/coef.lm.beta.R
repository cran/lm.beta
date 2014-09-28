coef.lm.beta <- function(object, standardized=TRUE, ...) {
  if(standardized) {
    print(object$standardized.coefficients,...)
  } else {
    print(object$coefficients,...)
  }
}