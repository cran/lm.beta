\name{print.lm.beta}
\alias{print.lm.beta}
\title{
Print \code{lm.beta}-object
}
\description{
S3-method \code{print} for object \code{lm.beta}.
}
\usage{
\S3method{print}{lm.beta}(x, standardized = TRUE, ...)
}
\arguments{
  \item{x}{
Object of class \code{lm.beta}.
}
  \item{standardized}{
Logical. Should the standardized values be printed?
}
  \item{\dots}{
Additional arguments to pass to \code{print}.
}
}
\details{
If \code{standardized=FALSE}, the standard \code{print.lm}-method is called, else (standard value) the regression coefficients are replaced by the standardized ones.

The additional arguments are in case of \code{standardized=FALSE} passed to \code{print.lm}, else they are passed to \code{print} for classes \code{call} and \code{vector}.
}
\value{
Original object.
}
\author{
Stefan Behrendt, \email{r@behrendt-stefan.de}
}
\seealso{
\code{\link{lm}} for creating the \code{lm}-object, \code{\link[lm.beta]{lm.beta}} for creating the demanded object and \code{\link{summary.lm.beta}}, \code{\link{coef.lm.beta}} for other overwritten S3-methods.
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
print(lm.D9.beta,standardized=FALSE)
}
