coef.lm.beta <- function(object, standardized=TRUE, ...) {
  if(standardized) {
    object$standardized.coefficients
  } else {
    object$coefficients
  }
}