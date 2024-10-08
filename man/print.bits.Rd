% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bits.R
\name{print.bits}
\alias{print.bits}
\title{Print a bits vector
Display the logical vector as a bit string where \code{FALSE} is shown as \code{0} and
\code{TRUE} as \code{1}.}
\usage{
\method{print}{bits}(x, ...)
}
\arguments{
\item{x}{the object to print}

\item{...}{currently ignored}
}
\description{
Print a bits vector
Display the logical vector as a bit string where \code{FALSE} is shown as \code{0} and
\code{TRUE} as \code{1}.
}
\examples{
z <- bits(c(FALSE, TRUE))
print(z)
}
\seealso{
Other bits: 
\code{\link{as.character.bits}()},
\code{\link{bits}()},
\code{\link{bits2int}()},
\code{\link{c.bits}()}
}
\author{
Thierry Onkelinx
}
\concept{bits}
