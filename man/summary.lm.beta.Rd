\name{summary.lm.beta}
\alias{summary.lm.beta}
\title{
Summarize Standardized Linear-Model-Object
}
\description{
S3-method \code{summary} for object \code{lm.beta}.
}
\usage{
\S3method{summary}{lm.beta}(object, standardized = TRUE, ...)
}
\arguments{
  \item{object}{An \R object of class \code{lm.beta}}
  \item{standardized}{Logical. Should the standardized values be integrated?}
  \item{\dots}{Additional arguments to pass to \code{summary.lm}}
}
\details{
If \code{standardized=FALSE}, the standard \code{summary.lm}-method is called, else (the standard value) the standardized regression coefficients are added into the coefficient table.

The additional arguments are passed to \code{summary.lm}.
}
\value{
Adapted \code{\link{summary.lm}}-object, in case of \code{standardized=TRUE} with additional class \code{summary.lm.beta}.
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
summary(lm.D9.beta)
summary(lm.D9.beta,standardized=FALSE)
}
