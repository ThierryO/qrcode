% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bits.R
\name{as.character.bits}
\alias{as.character.bits}
\title{Convert a bits object into a character string}
\usage{
\method{as.character}{bits}(x, ...)
}
\arguments{
\item{x}{the bits object}

\item{...}{currently ignore}
}
\description{
Convert a bits object into a character string
}
\examples{
z <- bits(c(FALSE, TRUE, TRUE, FALSE))
z
as.character(z)
}
\seealso{
Other bits: 
\code{\link{bits}()},
\code{\link{bits2int}()},
\code{\link{c.bits}()},
\code{\link{print.bits}()}
}
\author{
Thierry Onkelinx
}
\concept{bits}
