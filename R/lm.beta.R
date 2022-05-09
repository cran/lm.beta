lm.beta <- function(object, complete.standardization = FALSE) {
  if (!("lm" %in% attr(object, "class"))) stop("object has to be of class lm")
  i <- if (!complete.standardization) attr(attr(object$model, "terms"), "intercept") else 1
  object$standardized.coefficients <- coef(object) * apply(as.matrix(model.matrix(object)), 2, function(x) sqrt(sum((x - mean(x, na.rm = T) * i)^2, na.rm = T))) / apply(as.matrix(model.frame(object)[, 1]), 2, function(x) sqrt(sum((x - mean(x, na.rm = T) * i)^2, na.rm = T)))
  if (attr(attr(object$model, "terms"), "intercept") == 1) object$standardized.coefficients[1] <- NA
  attr(object, "class") <- c("lm.beta", "lm")
  return(object)
}

