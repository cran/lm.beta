summary.lm.beta <- function(object, standardized=TRUE, ...) {
  x.summary <- summary.lm(object,...)
  if(standardized) x.summary$coefficients <- cbind(x.summary$coefficients[,1,drop=F],Standardized=object$standardized.coefficients[rownames(x.summary$coefficients)],x.summary$coefficients[,-1,drop=F])
  x.summary
}