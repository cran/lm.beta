print.lm.beta <- function(x, standardized=TRUE, ...) {
  if(standardized) {
    cat("\nCall:\n")
    print(x$call,...)
    cat("\nStandardized Coefficients::\n")
    print(x$standardized.coefficients,...)
    cat("\n")
  } else {
    attr(x,"class") <- "lm"
    print(x,...)
    attr(x,"class") <- c("lm.beta","lm")
  }
  invisible(x)
}