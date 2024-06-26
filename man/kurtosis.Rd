% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic_stats.R
\name{kurtosis}
\alias{kurtosis}
\title{Kurtosis of a Time-series}
\usage{
kurtosis(.invariant, .method = c("excess", "moment", "fisher"), .na_rm = FALSE)
}
\arguments{
\item{.invariant}{An univariate or multivariate time series.}

\item{.method}{A \code{character} string. One of: \code{"excess"}, \code{"moment"} or \code{"fisher"}.}

\item{.na_rm}{A \code{logical} value. Should missing values be removed? The default is \code{FALSE}.}
}
\value{
A tidy \code{tibble} with 2 columns.
}
\description{
This function computes the kurtosis of a given data set. It was written for
internal computations.
}
\details{
The method can be either \code{"moment"}, \code{"fisher"}, or \code{"excess"}.
}
\examples{
if (FALSE) {
  kurtosis(diff(log(EuStockMarkets)), .method = 'excess')
}
}
\seealso{
\code{\link{skewness}}
}
\keyword{internal}
