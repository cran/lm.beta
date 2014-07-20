lm.beta <- function(object) {
  if(!"lm"%in%attr(object,"class")) stop("object has to be of class lm")
  model <- model.matrix(object)
  beta <- coef(object)[-1]*apply(as.matrix(model[,-1]),2,sd)/sd(object$model[,1])
  object$standardized.coefficients <- beta
  attr(object,"class") <- c("lm.beta","lm")
  return(object)
}