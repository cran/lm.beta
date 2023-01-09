\name{lm.beta}
\alias{lm.beta}
\title{
Add Standardized Regression Coefficients to Linear-Model-Objects
}
\description{
Adds standardized regression coefficients to objects created by \code{\link{lm}}.
}
\usage{
lm.beta(object, complete.standardization = FALSE)
}
\arguments{
  \item{object}{An \R object of type \code{lm}}
  \item{complete.standardization}{Logical. (See Details.)}
}
\details{
Calculates the standardized regression coefficients by common method used for example in SPSS. For translating the formula, functions \code{\link{model.matrix}} (for the right-hand side) and \code{\link{model.frame}} (for the left-hand side) are used. Additionally the case weights are regarded. So all options saved in the \code{\link{lm}}-object are supported.

In the case of models with intercept, the standardization results in the same estimates as \code{lm(..., data = scale(data))}.

In the case of models without intercept, there are two different types of standardization available. (1) Complete standardization (\code{complete.standardization = TRUE}) results in the same estimates as \code{lm(..., data = scale(data))} and therefore results in the same estimates as the same model with intercept. (2) Incomplete standardization (\code{complete.standardization = FALSE}, the standard value) results in the same estimates as \code{lm(..., data = scale(data, center = FALSE))}. This estimation is implemented in IBM SPSS Statistics. For a theoretical justification see \emph{Eisenhauer 2003}.

Please regard:

Package \code{lm.beta} standardizes the coefficients after estimating them using the standard deviations or similar measures of the used variables. So there are unstandardized and standardized coefficients available simultaneously.

Standardizing before estimating is not (yet) available in this package, but by using the function \code{scale} you can do this by using basic commands. Hereby please regard that the option \code{center} influences the way of interpretation of the intercept.

Package \code{lm.beta} standardizes all coefficients disregarding the use in interpretation. In this version, all types of scales of the variables (metrical, categorical, ...), all types of contrasts, interaction effects and additional terms on both sides of the formula can be handled if \code{lm} can handle them. The sensitive use in interpretation has to be regarded by the user.
}
\value{
A list of class \code{lm.beta} like a \code{\link{lm}}-object extended by
\itemize{
\item \strong{standardized.coefficients} named vector of the standardized coefficients.
}
}
\references{
Eisenhauer, J.G. (2003). Regression through the Origin. In \emph{Teching Statistics}, 25(3).

Urban, D., Mayerl, J., Sackmann, R. (Hrsg.) \emph{Regressionsanalyse : Theorie, Technik und Anwendung}. VS-Verlag, 4th ed.

Vittinghoff, E. et al (2005) \emph{Regression methods in biostatistics: Linear, logistic, survival, and repeated measures models}, Springer, p 75
}
\author{
Stefan Behrendt, \email{r@behrendt-stefan.de}
}
\note{
Some S3 methods, where standardized coefficients mind, are extended, the others work unchanged.
}

\seealso{
\code{\link{lm}} for creating the demanded object and \code{\link{print.lm.beta}}, \code{\link{summary.lm.beta}} and \code{\link{coef.lm.beta}} as well as \code{\link{xtable.lm.beta}} for extended S3-methods.
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
xtable::xtable(lm.D9.beta)
}
\keyword{lm}
\keyword{beta}
\keyword{standardized}
