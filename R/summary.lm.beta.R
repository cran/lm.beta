summary.lm.beta <- function(object, standardized=TRUE, ...) {
  x.summary <- summary.lm(object,...)
  if(standardized) x.summary$coefficients <- cbind(Estimate=x.summary$coefficients[,1],Standardized=c(NA,object$standardized.coefficients),x.summary$coefficients[,-1])
  x.summary
}