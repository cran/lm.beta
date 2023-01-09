\name{coef.lm.beta}
\alias{coef.lm.beta}
\title{
Print Coefficients of Standardized Linear-Model-Object
}
\description{
S3-method \code{coef} for object \code{lm.beta}.
}
\usage{
\S3method{coef}{lm.beta}(object, standardized = TRUE, ...)
}
\arguments{
  \item{object}{An \R object of class \code{lm.beta}}
  \item{standardized}{Logical. Should the standardized values be returned?}
  \item{\dots}{additional arguments. (Currently ignored.)}
}
\details{
If \code{standardized=FALSE}, the unstandardized regression coefficients are printed like if calling standard \code{coef.lm}-method, else (the standard value) the standardized regression coefficients are printed.
}
\value{
named numeric Vector of (un)standardized regression coefficients.
}
\author{
Stefan Behrendt, \email{r@behrendt-stefan.de}
}
\seealso{
\code{\link{lm.beta}} for creating the \code{lm.beta}-object.
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
coef(lm.D9.beta)
coef(lm.D9.beta,standardized=FALSE)
}
