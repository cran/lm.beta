\name{lm.beta}
\alias{lm.beta}
\title{
Add standardized regression coefficients to \code{lm}-objects
}
\description{
This function adds standardized regression coefficients to objects created by \code{\link{lm}}.
}
\usage{
lm.beta(object)
}
\arguments{
  \item{object}{
    Object of type \code{lm}.
}
}
\details{
Calculates the standardized regression coefficients by common method. For translating the formula, function \link{model.matrix} is used like in original \code{\link{lm}}-function and therefore supports all options saved in the object.
}
\value{
A list of class \code{lm.beta} like a \code{\link{lm}}-object extended by
\itemize{
\item \strong{standardized.coefficients}: Named vector of the standardized coefficients.
}
}
\references{
Vittinghoff, E. et al (2005) \emph{Regression methods in biostatistics: Linear, logistic, survival, and repeated measures models}, Springer, p 75
}
\author{
Stefan Behrendt, \email{r@behrendt-stefan.de}
}
\note{
The intercept cannot be standardized and is therefore excluded from the results.

Some S3 methods, where standardized coefficients mind, are overwritten, the others work unchanged.
}

\seealso{
\code{\link{lm}} for creating the demanded object and \code{\link{print.lm.beta}}, \code{\link{summary.lm.beta}}, \code{\link{coef.lm.beta}} for overwritten S3-methods.
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
\keyword{lm}
\keyword{beta}
\keyword{standardized}
