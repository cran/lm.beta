\name{lm.beta-package}
\alias{lm.beta-package}
\docType{package}
\title{
Add standardized regression coefficients to \code{lm}-objects
}
\description{
This package adds standardized regression coefficients to objects created by \code{\link{lm}}.

It also overwrites the S3 methods \code{print}, \code{summary} and \code{coef} with additional argument standardized.
}
\details{
\tabular{ll}{
Package: \tab lm.beta\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-07-20\cr
License: \tab GPL(>=2)\cr
}
}
\author{
Stefan Behrendt

Maintainer: Stefan Behrendt <r@behrendt-stefan.de>
}
\references{
Vittinghoff, E. et al (2005) \emph{Regression methods in biostatistics: Linear, logistic, survival, and repeated measures models}, Springer, p 75
}
\keyword{ package }
\seealso{
\code{\link[lm.beta]{lm.beta}}, \code{\link{lm}}
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
lm.D9.beta <- lm.beta(lm.D9)
print(lm.D9.beta)
summary(lm.D9.beta)
coef(lm.D9.beta)
}
