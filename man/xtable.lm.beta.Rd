\name{xtable.lm.beta}
\alias{xtable.lm.beta}
\alias{xtable.summary.lm.beta}
\title{
Coerce Coefficent Table of Standardized Linear-Model-Object to LaTeX- and HTML-Tables
}
\description{
S3-method \code{xtable} for object \code{lm.beta} and \code{summary.lm.beta}.
}
\usage{
\S3method{xtable}{lm.beta}(x, caption = NULL, label = NULL, align = NULL,
                           digits = NULL, display = NULL, auto = FALSE, ...)
\S3method{xtable}{summary.lm.beta}(x, caption = NULL, label = NULL, align = NULL,
                                   digits = NULL, display = NULL, auto = FALSE, ...)
}
\arguments{
  \item{x}{An \R object of class '\link{lm.beta}' or '\link{summary.lm.beta}'.}
  \item{caption}{see \code{\link{xtable}}}
  \item{label}{see \code{\link{xtable}}}
  \item{align}{see \code{\link{xtable}}}
  \item{digits}{see \code{\link{xtable}}}
  \item{display}{see \code{\link{xtable}}}
  \item{auto}{see \code{\link{xtable}}}
  \item{...}{Additional arguments. (Currently ignored.)}
}
\details{
see \code{\link{xtable}}
}
\value{
see \code{\link{xtable}}
}
\author{
Stefan Behrendt, \email{r@behrendt-stefan.de}
}
\seealso{
\code{\link{lm.beta}} for creating the \code{lm.beta}-object and \code{\link{xtable}} for creating the tables.
}
\examples{
## Taken from lm help
##
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)

# standardize
lm.D9.beta <- lm.beta(lm.D9)
xtable::xtable(lm.D9.beta)
}