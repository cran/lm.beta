xtable.lm.beta <- function(x, caption = NULL, label = NULL,
                           align = NULL, digits = NULL, display = NULL,
                           auto = FALSE, ...) {
  return(xtable.summary.lm.beta(summary(x, standardized = TRUE),
                                caption = caption, label = label, align = align,
                                digits = digits, display = display, auto = auto,
                                ...))
}

xtable.summary.lm.beta <- function(x, caption = NULL, label = NULL, align = NULL,
                                   digits = NULL, display = NULL, auto = FALSE,
                                   ...) {
  x <- data.frame(x$coef, check.names = FALSE)

  class(x) <- c("xtable", "data.frame")
  caption(x) <- caption
  label(x) <- label
  if(auto && is.null(align))   align   <- xalign(x)
  if(auto && is.null(digits))  digits  <- xdigits(x)
  if(auto && is.null(display)) display <- xdisplay(x)
  align(x) <- switch(1+is.null(align), align, c("r","r","r","r","r","r"))
  digits(x) <- switch(1+is.null(digits), digits, c(0,4,4,4,2,4))
  display(x) <- switch(1+is.null(display), display, c("s","f","f","f","f","f"))
  return(x)
}