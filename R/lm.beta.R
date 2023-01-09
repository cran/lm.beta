lm.beta <- function(object, complete.standardization = FALSE) {
  # stop if not lm-class
  # --> v2 will support selected glm's
  # --> v2 will support selected robust estimators
  if (!("lm" %in% attr(object, "class"))) stop("'object' has to be of class 'lm'")

  # set scaling for models without intercept -- see help page
  if (complete.standardization) {
    i <- 1
  } else {
    i <- attr(attr(object$model, "terms"), "intercept")
  }

  # determine n
  n <- nrow(object$model)

  # extract weights from 'object'
  if (exists("weights", object)) {
    w <- object$weights
  } else {
    w <- rep(1, n)
  }

  # determine weighted n
  nw <- sum(w != 0, na.rm = T)

  # extract LHS from 'object'...
  y <- as.matrix(model.frame(object)[, 1])
  # ... and determine its standard deviation
  sy <- sqrt(apply(y, 2,
                   function(a) {
                     sum(w * (a - weighted.mean(a, w, na.rm = T) * i) ^ 2, na.rm = T)
                   }) / ((nw - 1) / nw * sum(w, na.rm = T)))

  # extract RHS from 'object'...
  x <- as.matrix(model.matrix(object))
  # ... and determine its standard deviation
  sx <- sqrt(apply(x, 2,
                   function(a) {
                     sum(w * (a - weighted.mean(a, w, na.rm = T) * i) ^ 2, na.rm = T)
                   }) / ((nw - 1) / nw * sum(w, na.rm = T)))

  # estimate standardized coefficients
  object$standardized.coefficients <- coef(object) * sx / sy

  # set standardized intercept to 'NA'
  if (attr(attr(object$model, "terms"), "intercept") == 1) object$standardized.coefficients[1] <- NA

  # set class and return 'object'
  attr(object, "class") <- c("lm.beta", "lm")
  return(object)
}

