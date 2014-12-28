\name{lm.beta-package}
\alias{lm.beta-package}
\docType{package}
\title{
Add Standardized Regression Coefficients to \code{lm}-Objects
}
\description{
Adds standardized regression coefficients to objects created by \code{\link{lm}}.

Also extends the S3 methods \code{print}, \code{summary} and \code{coef} with additional boolean argument standardized.

Please regard:

Package \code{lm.beta} works in the way of common statistical softwares like SPSS by standardizing the coefficients after estimating them using the standard deviations or similar measures of the used variables. So there are unstandardized and standardized coefficients available simultaneously.

Standardizing before estimating is not (yet) available in this package, but by using the command \code{scale} you can do this by using basic commands. Hereby please regard that the option \code{center} influences the way of interpretation of the intercept. 

Package \code{lm.beta} standardizes all coefficients disregarding the use in interpretation. In this version, all types of scales of the variables (metrical, categorical, ...), all types of contrasts, interaction effects and additional terms on both sides of the formula can be handled if \code{lm} can handle them. The sensitive use in interpretation has to be regarded by the user.
}
\details{
\tabular{ll}{
Package: \tab lm.beta\cr
Type: \tab Package\cr
Version: \tab 1.5-1\cr
Date: \tab 2014-12-28\cr
License: \tab GPL(>=2)\cr
}
}
\author{
Stefan Behrendt <r@behrendt-stefan.de>
}
\references{
Urban, D., Mayerl, J., Sackmann, R. (Hrsg.) \emph{Regressionsanalyse : Theorie, Technik und Anwendung}, VS-Verlag, 4. Aufl.

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

# standardize
lm.D9.beta <- lm.beta(lm.D9)
print(lm.D9.beta)
summary(lm.D9.beta)
coef(lm.D9.beta)
}
