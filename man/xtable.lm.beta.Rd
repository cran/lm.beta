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
\S3method{xtable}{lm.beta}(x, standardized = TRUE, caption = NULL, label = NULL,
                           align = NULL, digits = NULL, display = NULL,
                           auto = FALSE, ...)
\S3method{xtable}{summary.lm.beta}(x, caption = NULL, label = NULL, align = NULL,
                                   digits = NULL, display = NULL, auto = FALSE,
                                   ...)
}
\arguments{
  \item{x}{An \R object of class '\link{lm.beta}' or '\link{summary.lm.beta}'.}
  \item{standardized}{Logical. Should the standardized values be returned?}
  \item{caption}{Character vector of length 1 or 2 containing the
    table's caption or title.  If length is 2, the second item is the
    "short caption" used when LaTeX generates a "List of Tables". Set to
    \code{NULL} to suppress the caption.  Default value is \code{NULL}.}
  \item{label}{Character vector of length 1 containing the LaTeX label
    or HTML anchor. Set to \code{NULL} to suppress the label.  Default
    value is \code{NULL}.}
  \item{align}{Character vector of length equal to the number of columns
    of the resulting table, indicating the alignment of the corresponding
    columns.  Also, \code{"|"} may be used to produce vertical lines
    between columns in LaTeX tables, but these are effectively ignored
    when considering the required length of the supplied vector.  If a
    character vector of length one is supplied, it is split as
    \code{strsplit(align, "")[[1]]} before processing. Since the row
    names are printed in the first column, the length of \code{align} is
    one greater than \code{ncol(x)} if \code{x} is a
    \code{data.frame}. Use \code{"l"}, \code{"r"}, and \code{"c"} to
    denote left, right, and center alignment, respectively.  Use
    \code{"p{3cm}"} etc. for a LaTeX column of the specified width. For
    HTML output the \code{"p"} alignment is interpreted as \code{"l"},
    ignoring the width request. Default depends on the class of
    \code{x}.}
  \item{digits}{
    Numeric vector of length equal to one (in which case it will be
    replicated as necessary) or to the number of columns of the
    resulting table \bold{or} matrix of the same size as the resulting
    table, indicating the number of digits to display in the
    corresponding columns. Since the row names are printed in the first
    column, the length of the vector \code{digits} or the number of
    columns of the matrix \code{digits} is one greater than
    \code{ncol(x)} if \code{x} is a \code{data.frame}. Default depends
    on the class of \code{x}. If values of \code{digits} are negative, the
    corresponding values of \code{x} are displayed in scientific format
    with \code{abs(digits)} digits.}
  \item{display}{
    Character vector of length equal to the number of columns of the
    resulting table, indicating the format for the corresponding columns.
    Since the row names are printed in the first column, the length of
    \code{display} is one greater than \code{ncol(x)} if \code{x} is a
    \code{data.frame}.  These values are passed to the \code{formatC}
    function.  Use \code{"d"} (for integers), \code{"f"}, \code{"e"},
    \code{"E"}, \code{"g"}, \code{"G"}, \code{"fg"} (for reals), or
    \code{"s"} (for strings).  \code{"f"} gives numbers in the usual
    \code{xxx.xxx} format; \code{"e"} and \code{"E"} give
    \code{n.ddde+nn} or \code{n.dddE+nn} (scientific format); \code{"g"}
    and \code{"G"} put \code{x[i]} into scientific format only if it
    saves space to do so.  \code{"fg"} uses fixed format as \code{"f"},
    but \code{digits} as number of \emph{significant} digits.  Note that
    this can lead to quite long result strings.  Default depends on the
    class of \code{x}.}
  \item{auto}{
    Logical, indicating whether to apply automatic format when no value
    is passed to \code{align}, \code{digits}, or \code{display}. This
    \sQuote{autoformat} (based on \code{xalign}, \code{xdigits}, and
    \code{xdisplay}) can be useful to quickly format a typical
    \code{matrix} or \code{data.frame}. Default value is \code{FALSE}.}
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
xtable::xtable(lm.D9.beta)
}